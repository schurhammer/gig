import anf/ab_normal_form as anf
import anf/imperative_form
import anf/monadic_form
import gleam/order
import glexer

import gig/closure.{
  type CustomType, type Field, type Function, type Module, type Variant,
}
import gig/core
import gig/graph
import gig/mono
import gig/type_graph

import gleam/int
import gleam/list
import gleam/set
import gleam/string

const c_keywords = [
  "auto", "break", "case", "char", "const", "continue", "default", "do",
  "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline",
  "int", "long", "register", "restrict", "return", "short", "signed", "sizeof",
  "static", "struct", "switch", "typedef", "union", "unsigned", "void",
  "volatile", "while", "asm", "typeof", "main", "printf", "malloc", "free",
  "exit", "abort",
]

fn is_keyword(name: String) -> Bool {
  list.contains(c_keywords, name)
}

fn escape_keywords(name: String) -> String {
  case is_keyword(name) {
    True -> name <> "_"
    False -> name
  }
}

fn generate_type(typ: mono.Type) {
  mono.type_name(typ)
}

fn generate_field(field: Field, indent: String) -> String {
  indent
  <> generate_type(field.typ)
  <> " "
  <> escape_keywords(field.name)
  <> ";\n"
}

fn generate_fields(fields: List(Field), indent: String) -> String {
  list.map(fields, generate_field(_, indent))
  |> string.concat()
}

fn generate_enum(t: CustomType) -> String {
  let values =
    list.map(t.variants, fn(v) { "  E_" <> v.untyped_name })
    |> string.join(",\n")
  "enum E_" <> t.untyped_name <> " {\n" <> values <> "\n};"
}

fn generate_union(t: CustomType) -> String {
  let fields =
    list.map(t.variants, fn(v) {
      "  struct " <> v.name <> " *" <> v.display_name <> ";"
    })
    |> string.join("\n")
  "union U_" <> t.name <> " {\n" <> fields <> "\n};"
}

fn generate_record_struct(v: Variant) -> String {
  let field_str = generate_fields(v.fields, "  ")
  "struct " <> v.name <> " {\n" <> field_str <> "};"
}

fn generate_sumtype_struct(t: CustomType) {
  "struct T_"
  <> t.name
  <> " {\n  enum E_"
  <> t.untyped_name
  <> " tag;\n  union U_"
  <> t.name
  <> " val;\n};"
}

fn generate_record_typedef(t: CustomType, v: Variant) -> String {
  case t.pointer {
    True -> "typedef struct " <> v.name <> " *" <> t.name <> ";"
    False -> "typedef struct " <> v.name <> " " <> t.name <> ";"
  }
}

fn generate_sumtype_typedef(t: CustomType) -> String {
  "typedef struct T_" <> t.name <> " " <> t.name <> ";"
}

fn generate_parameters(parameters: List(Field)) -> String {
  case parameters {
    [] -> ""
    _ -> {
      let param_strs =
        list.map(parameters, fn(p) {
          generate_type(p.typ) <> " " <> escape_keywords(p.name)
        })
      string.join(param_strs, ", ")
    }
  }
}

fn generate_term(term: anf.Term, indent: String) -> String {
  case term {
    anf.Value(value) -> indent <> "return " <> generate_value(value, "") <> ";"
    anf.Call(function, args) -> {
      indent <> generate_call("return ", function, args)
    }
    anf.CallClosure(function, args) -> {
      generate_call_closure("return ", function, args, indent)
    }
    anf.BeginTerm(statements, term) -> {
      list.map(statements, generate_statement(_, indent))
      |> list.filter(fn(s) { s != "" })
      |> string.join("\n")
      <> "\n"
      <> generate_term(term, indent)
    }
    anf.IfTerm(condition, then_term, else_term) -> {
      generate_if(
        condition,
        generate_term(then_term, indent <> "  "),
        generate_term(else_term, indent <> "  "),
        indent,
      )
    }
    anf.PanicTerm(arg) -> {
      indent <> generate_panic(arg)
    }
  }
}

fn generate_value(value: anf.Value, _: String) -> String {
  case value {
    anf.Literal(_typ, lit) -> generate_literal(lit)
    anf.Var(_typ, name) -> escape_keywords(name)
    anf.Op(_typ, op, args) -> generate_op(op, args)
  }
}

fn integer_literal(val: String) -> String {
  let clean_val = string.replace(val, "_", "")

  case clean_val {
    // Binary literals (0b) - convert to decimal
    "0b" <> binary_digits -> {
      case binary_to_decimal(binary_digits) {
        Ok(decimal) -> decimal
        Error(_) -> clean_val
      }
    }
    // Octal literals (0o) - convert to C octal format
    "0o" <> octal_digits -> "0" <> octal_digits
    // Hex literals (0x) - already C compatible
    "0x" <> _ -> clean_val
    // Regular numbers
    _ -> clean_val
  }
}

fn binary_to_decimal(binary_str: String) -> Result(String, Nil) {
  case int.base_parse(binary_str, 2) {
    Ok(value) -> Ok(int.to_string(value))
    Error(_) -> Error(Nil)
  }
}

fn float_literal(val: String) -> String {
  string.replace(val, "_", "") <> "L"
}

fn generate_literal(lit: core.LiteralKind) -> String {
  case lit {
    core.NilVal -> "0"
    core.Bool(b) -> b
    core.Int(i) -> integer_literal(i)
    core.Float(f) -> float_literal(f)
    core.String(s) -> {
      let s = string.replace(s, "\n", "\\n")
      let assert Ok(unescaped) = glexer.unescape_string(s)
      let size = int.to_string(string.byte_size(unescaped))
      "new_String(\"" <> s <> "\", " <> size <> ")"
    }
    core.BitArray(size) -> "new_bit_array(" <> size <> ")"
  }
}

fn function_type_cast(fun: String, args: List(String), return: String) -> String {
  "((" <> return <> " (*)(" <> string.join(args, ", ") <> "))" <> fun <> ")"
}

fn generate_if(
  condition: anf.Value,
  then_branch: String,
  else_branch: String,
  indent: String,
) -> String {
  indent
  <> "if ("
  <> generate_value(condition, "")
  <> ") {\n"
  <> then_branch
  <> "\n"
  <> indent
  <> "} else {\n"
  <> else_branch
  <> "\n"
  <> indent
  <> "}"
}

fn generate_call(
  target: String,
  func: anf.Value,
  args: List(anf.Value),
) -> String {
  let arg_strs = list.map(args, generate_value(_, ""))
  target
  <> generate_value(func, "")
  <> "("
  <> string.join(arg_strs, ", ")
  <> ");"
}

fn generate_op(op: mono.Op, args: List(anf.Value)) -> String {
  let args = list.map(args, generate_value(_, ""))

  case op {
    mono.FieldAccess(kind, variant, field) -> {
      case args {
        [obj] -> {
          let accessor = case kind {
            mono.StructPointerAccess -> "->"
            mono.StructAccess -> "."
            mono.TaggedUnionAccess -> ".val." <> variant <> "->"
          }
          obj <> accessor <> escape_keywords(field)
        }
        _ -> "/* invalid field access */"
      }
    }
    mono.VariantCheck(variant) -> {
      case args {
        [obj] -> obj <> ".tag == E_" <> variant

        _ -> "/* invalid variant check */"
      }
    }
  }
}

fn generate_panic(arg: anf.Value) {
  "panic_exit(" <> generate_value(arg, "") <> ");"
}

fn generate_call_closure(
  target: String,
  func: anf.Value,
  args: List(anf.Value),
  indent: String,
) -> String {
  let assert mono.FunctionType(arg_types, return_type) = func.typ
  let arg_types = list.map(arg_types, fn(a) { mono.type_name(a) })
  let return_type = mono.type_name(return_type)

  let arg_values = list.map(args, generate_value(_, ""))

  let closure = generate_value(func, "")
  let fun = closure <> ".fun"
  let env = closure <> ".env"

  let call_with_env =
    function_type_cast(fun, ["void *", ..arg_types], return_type)
    <> "("
    <> string.join([env, ..arg_values], ", ")
    <> ");"

  let call_without_env =
    function_type_cast(fun, arg_types, return_type)
    <> "("
    <> string.join(arg_values, ", ")
    <> ");"

  let inner_indent = indent <> "  "
  indent
  <> "if (is_closure("
  <> closure
  <> ")) {\n"
  <> inner_indent
  <> target
  <> call_with_env
  <> "\n"
  <> indent
  <> "} else {\n"
  <> inner_indent
  <> target
  <> call_without_env
  <> "\n"
  <> indent
  <> "}"
}

fn generate_setter(var: String) {
  case var {
    "_" <> _ -> ""
    _ -> escape_keywords(var) <> " = "
  }
}

fn generate_statement(stmt: anf.Statement, indent: String) -> String {
  case stmt {
    anf.Begin(statements) -> {
      list.map(statements, generate_statement(_, indent))
      |> list.filter(fn(s) { s != "" })
      |> string.join("\n")
    }
    anf.Declare("_" <> _, _) -> {
      ""
    }
    anf.Declare(var, typ) -> {
      indent <> generate_type(typ) <> " " <> escape_keywords(var) <> ";"
    }
    anf.SetValue(var, value) -> {
      indent <> generate_setter(var) <> generate_value(value, "") <> ";"
    }
    anf.SetCall(var, func, args) -> {
      indent <> generate_call(generate_setter(var), func, args)
    }
    anf.SetCallClosure(var, func, args) -> {
      generate_call_closure(generate_setter(var), func, args, indent)
    }
    anf.If(condition, then_stmt, else_stmt) -> {
      generate_if(
        condition,
        generate_statement(then_stmt, indent <> "  "),
        generate_statement(else_stmt, indent <> "  "),
        indent,
      )
    }
    anf.Panic(arg) -> {
      indent <> generate_panic(arg)
    }
  }
}

fn generate_function(f: Function) -> String {
  let body =
    f.body
    |> monadic_form.mf()
    |> imperative_form.mcg()
    |> anf.ab_normalise()
  let return_type = generate_type(f.return)
  let params = generate_parameters(f.parameters)
  let body = generate_term(body, "  ")

  return_type
  <> " "
  <> f.name
  <> "("
  <> params
  <> ") {\n"
  <> body
  <> "\n"
  <> "}\n"
}

fn generate_constructor_header(t: CustomType, v: Variant) -> String {
  case v.fields {
    [] -> "extern const " <> t.name <> " new_" <> v.name
    _ -> {
      let params =
        v.fields
        |> list.map(fn(f) {
          generate_type(f.typ) <> " " <> escape_keywords(f.name)
        })
        |> string.join(", ")
      t.name <> " new_" <> v.name <> "(" <> params <> ")"
    }
  }
}

fn generate_record_constructor_body(t: CustomType, v: Variant) -> String {
  let field_assignments =
    "{"
    <> v.fields
    |> list.map(fn(f) {
      "." <> escape_keywords(f.name) <> " = " <> escape_keywords(f.name)
    })
    |> string.join(", ")
    <> "};"

  case t.pointer {
    True ->
      case v.fields {
        [] -> "  return 0"
        _ -> {
          "  "
          <> t.name
          <> " _val = malloc(sizeof(struct "
          <> v.name
          <> "));\n  *_val = "
          <> "(struct "
          <> v.name
          <> ")"
          <> field_assignments
          <> "\n  return _val;"
        }
      }
    False ->
      case v.fields {
        [] -> "  return (" <> t.name <> "){0};"
        _ -> {
          "  return (" <> t.name <> ")" <> field_assignments
        }
      }
  }
}

fn generate_sumtype_constructor_body(t: CustomType, v: Variant) -> String {
  case v.fields {
    [] ->
      "  return ("
      <> t.name
      <> "){.tag = E_"
      <> v.untyped_name
      <> ", .val = {0}};"
    _ -> {
      let variant_fields =
        v.fields
        |> list.map(fn(f) {
          "." <> escape_keywords(f.name) <> " = " <> escape_keywords(f.name)
        })
        |> string.join(", ")

      let malloc_assign =
        "  struct "
        <> v.name
        <> " *_ptr = malloc(sizeof(struct "
        <> v.name
        <> "));\n"
      let field_assign =
        "  *_ptr = (struct " <> v.name <> "){" <> variant_fields <> "};\n"
      let return_stmt =
        "  return ("
        <> t.name
        <> "){.tag = E_"
        <> v.untyped_name
        <> ", .val."
        <> v.display_name
        <> " = _ptr};"

      malloc_assign <> field_assign <> return_stmt
    }
  }
}

fn generate_sumtype_constructor(t: CustomType, v: Variant) -> String {
  let header = generate_constructor_header(t, v)
  case v.fields {
    [] ->
      "const "
      <> t.name
      <> " new_"
      <> v.name
      <> " = {.tag = E_"
      <> v.untyped_name
      <> ", .val = {0}};"
    _ -> {
      let body = generate_sumtype_constructor_body(t, v)
      header <> " {\n" <> body <> "\n}"
    }
  }
}

fn generate_record_constructor(t: CustomType, v: Variant) -> String {
  let header = generate_constructor_header(t, v)
  case v.fields {
    [] -> "const " <> t.name <> " new_" <> t.name <> " = {0};"
    _ -> {
      let body = generate_record_constructor_body(t, v)
      header <> " {\n" <> body <> "\n}"
    }
  }
}

fn generate_constructor_forward(t: CustomType, v: Variant) -> String {
  case v.fields {
    [] -> "extern const " <> t.name <> " new_" <> v.name <> ";"
    _ -> {
      let params =
        v.fields
        |> list.map(fn(f) {
          generate_type(f.typ) <> " " <> escape_keywords(f.name)
        })
        |> string.join(", ")
      t.name <> " new_" <> v.name <> "(" <> params <> ");"
    }
  }
}

fn generate_function_header(f: Function) -> String {
  generate_type(f.return)
  <> " "
  <> f.name
  <> "("
  <> generate_parameters(f.parameters)
  <> ");"
}

fn generate_eq_header(t: CustomType) {
  "Bool eq_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b)"
}

fn generate_field_equality_check(
  field: Field,
  access_op: String,
  indent: String,
) -> String {
  let escaped_name = escape_keywords(field.name)
  let field_typ = mono.type_name(field.typ)
  indent
  <> "if (!eq_"
  <> field_typ
  <> "(a"
  <> access_op
  <> escaped_name
  <> ", b"
  <> access_op
  <> escaped_name
  <> "))\n"
  <> indent
  <> "  return False;\n"
}

fn generate_record_eq_body(t: CustomType, v: Variant) -> String {
  let access = case t.pointer {
    True -> "->"
    False -> "."
  }
  list.map(v.fields, generate_field_equality_check(_, access, "  "))
  |> string.concat()
  <> "  return True;\n"
}

fn generate_sumtype_eq_body(typ: CustomType) -> String {
  "  if (a.tag != b.tag) {\n"
  <> "    return False;\n"
  <> "  }\n"
  <> "  switch (a.tag) {\n"
  <> list.map(typ.variants, fn(v) {
    let access = ".val." <> v.display_name <> "->"
    "  case E_"
    <> v.untyped_name
    <> ":\n"
    <> list.map(v.fields, generate_field_equality_check(_, access, "    "))
    |> string.concat()
    <> "    return True;\n"
  })
  |> string.join("")
  <> "  default:\n"
  <> "    return True;\n"
  <> "  }\n"
}

fn generate_record_eq(t: CustomType, v: Variant) {
  generate_eq_header(t) <> " {\n" <> generate_record_eq_body(t, v) <> "}"
}

fn generate_sumtype_eq(t: CustomType) {
  generate_eq_header(t) <> " {\n" <> generate_sumtype_eq_body(t) <> "}"
}

fn generate_lt_header(t: CustomType) {
  "Bool lt_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b)"
}

fn generate_field_lt_check(
  field: Field,
  access_op: String,
  indent: String,
) -> String {
  let escaped_name = escape_keywords(field.name)
  let field_typ = mono.type_name(field.typ)
  indent
  <> "if (!eq_"
  <> field_typ
  <> "(a"
  <> access_op
  <> escaped_name
  <> ", b"
  <> access_op
  <> escaped_name
  <> "))\n"
  <> indent
  <> "  return lt_"
  <> field_typ
  <> "(a"
  <> access_op
  <> escaped_name
  <> ", b"
  <> access_op
  <> escaped_name
  <> ");\n"
}

fn generate_record_lt_body(t: CustomType, v: Variant) -> String {
  let access = case t.pointer {
    True -> "->"
    False -> "."
  }
  list.map(v.fields, generate_field_lt_check(_, access, "  "))
  |> string.concat()
  <> "  return False;\n"
}

fn generate_sumtype_lt_body(typ: CustomType) -> String {
  "  if (a.tag != b.tag) {\n"
  <> "    return a.tag < b.tag;\n"
  <> "  }\n"
  <> "  switch (a.tag) {\n"
  <> list.map(typ.variants, fn(v) {
    let access = ".val." <> v.display_name <> "->"
    "  case E_"
    <> v.untyped_name
    <> ":\n"
    <> list.map(v.fields, generate_field_lt_check(_, access, "    "))
    |> string.concat()
    <> "    return False;\n"
  })
  |> string.join("")
  <> "  default:\n"
  <> "    return False;\n"
  <> "  }\n"
}

fn generate_record_lt(t: CustomType, v: Variant) {
  generate_lt_header(t) <> " {\n" <> generate_record_lt_body(t, v) <> "}"
}

fn generate_sumtype_lt(t: CustomType) {
  generate_lt_header(t) <> " {\n" <> generate_sumtype_lt_body(t) <> "}"
}

fn generate_inspect_header(t: CustomType) {
  "String inspect_" <> t.name <> "(" <> t.name <> " value)"
}

fn generate_field_inspect(field: Field, access_op: String) -> String {
  let escaped_name = escape_keywords(field.name)
  let field_typ = mono.type_name(field.typ)
  "inspect_" <> field_typ <> "(value" <> access_op <> escaped_name <> ")"
}

fn generate_record_inspect_body(t: CustomType, v: Variant) -> String {
  let access = case t.pointer {
    True -> "->"
    False -> "."
  }
  let field_inspects = case v.fields {
    [] -> ""
    fields -> {
      list.map(fields, fn(field) {
        let field_inspect = generate_field_inspect(field, access)
        "  result = append_string(result, " <> field_inspect <> ");\n"
      })
      |> string.join(
        "  result = append_string(result, new_String(\", \", 2));\n",
      )
    }
  }
  "  String result = new_String(\""
  <> v.display_name
  <> "(\", "
  <> string.length(v.display_name <> "(") |> int.to_string()
  <> ");\n"
  <> field_inspects
  <> "  result = append_string(result, new_String(\")\", 1));\n"
  <> "  return result;\n"
}

fn generate_sumtype_inspect_body(typ: CustomType) -> String {
  "  String result;\n"
  <> "  switch (value.tag) {\n"
  <> list.map(typ.variants, fn(v) {
    let access = ".val." <> v.display_name <> "->"
    let field_inspects = case v.fields {
      [] ->
        "    return new_String(\""
        <> v.display_name
        <> "\", "
        <> string.length(v.display_name) |> int.to_string()
        <> ");\n"
      fields -> {
        let field_lines =
          list.map(fields, fn(field) {
            let field_inspect = generate_field_inspect(field, access)
            "    result = append_string(result, " <> field_inspect <> ");\n"
          })
          |> string.join(
            "    result = append_string(result, new_String(\", \", 2));\n",
          )

        "    result = new_String(\""
        <> v.display_name
        <> "(\", "
        <> string.length(v.display_name <> "(") |> int.to_string()
        <> ");\n"
        <> field_lines
        <> "    result = append_string(result, new_String(\")\", 1));\n"
        <> "    return result;\n"
      }
    }
    "  case E_" <> v.untyped_name <> ":\n" <> field_inspects
  })
  |> string.join("")
  <> "  default:\n"
  <> "    return new_String(\"???\", 3);\n"
  <> "  }\n"
}

fn generate_record_inspect(t: CustomType, v: Variant) {
  generate_inspect_header(t)
  <> " {\n"
  <> generate_record_inspect_body(t, v)
  <> "}"
}

fn generate_sumtype_inspect(t: CustomType) {
  generate_inspect_header(t)
  <> " {\n"
  <> generate_sumtype_inspect_body(t)
  <> "}"
}

type Context {
  Context(
    generated_enums: set.Set(String),
    type_decl: List(String),
    type_impl: List(String),
    fun_decl: List(String),
    fun_impl: List(String),
    used_builtin: set.Set(String),
  )
}

fn is_builtin_used(c: Context, name: String) -> Bool {
  set.contains(c.used_builtin, name)
}

fn record_type(c: Context, t: CustomType, v: Variant) -> Context {
  let typedef = generate_record_typedef(t, v)
  let struct = generate_record_struct(v)

  let cons_decl = generate_constructor_forward(t, v)
  let cons_impl = generate_record_constructor(t, v)

  let eq_decl = generate_eq_header(t) <> ";"
  let eq_impl = case is_builtin_used(c, "eq_" <> t.name) {
    True -> generate_record_eq(t, v)
    False -> ""
  }

  let lt_decl = generate_lt_header(t) <> ";"
  let lt_impl = case is_builtin_used(c, "lt_" <> t.name) {
    True -> generate_record_lt(t, v)
    False -> ""
  }

  let inspect_decl = generate_inspect_header(t) <> ";"
  let inspect_impl = case is_builtin_used(c, "inspect_" <> t.name) {
    True -> generate_record_inspect(t, v)
    False -> ""
  }

  let fun_decl = [inspect_decl, lt_decl, eq_decl, cons_decl, ..c.fun_decl]
  let fun_impl = [inspect_impl, lt_impl, eq_impl, cons_impl, ..c.fun_impl]
  let type_decl = [typedef, ..c.type_decl]
  let type_impl = [struct, ..c.type_impl]

  Context(..c, type_decl:, type_impl:, fun_decl:, fun_impl:)
}

fn sumtype_type(c: Context, t: CustomType) -> Context {
  let typedef = generate_sumtype_typedef(t)

  let enum = case set.contains(c.generated_enums, t.untyped_name) {
    True -> ""
    False -> generate_enum(t)
  }
  let generated_enums = set.insert(c.generated_enums, t.untyped_name)

  let union = generate_union(t)

  let struct = generate_sumtype_struct(t)

  let structs =
    list.map(t.variants, fn(v) { generate_record_struct(v) })
    |> string.join("\n")

  let cons_decl =
    list.map(t.variants, fn(v) { generate_constructor_forward(t, v) })
    |> string.join("\n")

  let cons_impl =
    t.variants
    |> list.map(fn(v) { generate_sumtype_constructor(t, v) })
    |> string.join("\n")

  let eq_decl = generate_eq_header(t) <> ";"
  let eq_impl = case is_builtin_used(c, "eq_" <> t.name) {
    True -> generate_sumtype_eq(t)
    False -> ""
  }

  let lt_decl = generate_lt_header(t) <> ";"
  let lt_impl = case is_builtin_used(c, "lt_" <> t.name) {
    True -> generate_sumtype_lt(t)
    False -> ""
  }

  let inspect_decl = generate_inspect_header(t) <> ";"
  let inspect_impl = case is_builtin_used(c, "inspect_" <> t.name) {
    True -> generate_sumtype_inspect(t)
    False -> ""
  }

  let fun_decl = [inspect_decl, lt_decl, eq_decl, cons_decl, ..c.fun_decl]
  let fun_impl = [inspect_impl, lt_impl, eq_impl, cons_impl, ..c.fun_impl]
  let type_decl = [struct, union, enum, typedef, ..c.type_decl]
  let type_impl = [structs, ..c.type_impl]

  Context(..c, type_decl:, type_impl:, fun_decl:, fun_impl:, generated_enums:)
}

fn sort_types_by_dependency(mod: Module) -> List(CustomType) {
  let type_graph = type_graph.create(mod)
  let type_groups = graph.strongly_connected_components(type_graph)

  list.flatten(type_groups)
  |> list.map(fn(name) {
    let assert Ok(x) = list.find(mod.types, fn(x) { x.name == name })
    x
  })
  |> list.sort(fn(a, b) {
    case a.pointer, b.pointer {
      True, True -> order.Eq
      False, False -> order.Eq
      True, False -> order.Gt
      False, True -> order.Lt
    }
  })
}

pub fn compile_module(mod: Module) -> String {
  let types =
    sort_types_by_dependency(mod)
    |> list.filter(fn(t) { t.name != "Nil" && t.name != "Bool" })

  let c =
    Context(
      generated_enums: set.new(),
      used_builtin: mod.used_builtin,
      type_decl: [],
      type_impl: [],
      fun_decl: [],
      fun_impl: [],
    )

  let c =
    list.fold(types, c, fn(c, t) {
      case t.variants {
        [v] -> record_type(c, t, v)
        _ -> sumtype_type(c, t)
      }
    })

  let c =
    list.fold(mod.externals, c, fn(c, f) {
      let fun_decl = generate_function_header(f)
      let fun_decl = [fun_decl, ..c.fun_decl]
      Context(..c, fun_decl:)
    })

  let c =
    list.fold(mod.functions, c, fn(c, f) {
      let fun_decl = generate_function_header(f)
      let fun_impl = generate_function(f)

      let fun_decl = [fun_decl, ..c.fun_decl]
      let fun_impl = [fun_impl, ..c.fun_impl]
      Context(..c, fun_decl:, fun_impl:)
    })

  list.flatten([
    list.reverse(c.type_decl),
    list.reverse(c.type_impl),
    ["\n"],
    list.reverse(c.fun_decl),
    ["\n"],
    list.reverse(c.fun_impl),
  ])
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n")
}

pub fn compile_module_header(mod: Module) -> String {
  let types =
    sort_types_by_dependency(mod)
    |> list.filter(fn(t) { t.name != "Nil" && t.name != "Bool" })

  let c =
    Context(
      generated_enums: set.new(),
      used_builtin: mod.used_builtin,
      type_decl: [],
      type_impl: [],
      fun_decl: [],
      fun_impl: [],
    )

  let c =
    list.fold(types, c, fn(c, t) {
      case t.variants {
        [v] -> record_type(c, t, v)
        _ -> sumtype_type(c, t)
      }
    })

  let c =
    list.fold(mod.externals, c, fn(c, f) {
      let fun_decl = generate_function_header(f)
      let fun_decl = [fun_decl, ..c.fun_decl]
      Context(..c, fun_decl:)
    })

  list.flatten([
    list.reverse(c.type_decl),
    list.reverse(c.type_impl),
    ["\n"],
    list.reverse(c.fun_decl),
  ])
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n")
}
