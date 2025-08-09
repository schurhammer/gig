import gig/closure.{type CustomType, type Function, type Module}
import gig/core.{BitArray, Float, Int, String}
import gig/mono.{type_name}
import gig/normalise.{
  type Term, type Value, Call, CallClosure, If, Let, Literal, Op, Value,
  Variable,
}

import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/string

import gig/graph
import gig/type_graph

/// List of C keywords that cannot be used as variable names or function parameters
const c_keywords = [
  "auto", "break", "case", "char", "const", "continue", "default", "do",
  "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline",
  "int", "long", "register", "restrict", "return", "short", "signed", "sizeof",
  "static", "struct", "switch", "typedef", "union", "unsigned", "void",
  "volatile", "while", "asm", "typeof", "main", "printf", "malloc", "free",
  "exit", "abort",
]

type Context {
  Context(types: dict.Dict(String, CustomType))
}

/// Check if a given name is a C keyword or reserved identifier
fn is_keyword(name: String) -> Bool {
  list.contains(c_keywords, name)
}

/// Escape a name if it's a C keyword by appending an underscore
/// If the name is not a keyword, return it unchanged
fn escape_if_keyword(name: String) -> String {
  case is_keyword(name) {
    True -> name <> "_"
    False -> name
  }
}

fn hit_target(target: String, with: String) {
  case target {
    // no target means it is part of an expression
    "" -> with
    // discarded, no need to set the target
    "_" <> _ -> with <> ";\n"
    "RETURN" -> "return " <> with <> ";\n"
    target -> target <> " = " <> with <> ";\n"
  }
}

fn convert_float_literal(val: String) -> String {
  string.replace(val, "_", "") <> "L"
}

fn convert_integer_literal(val: String) -> String {
  let clean_val = string.replace(val, "_", "")

  case clean_val {
    // Binary literals (0b) - convert to decimal
    "0b" <> binary_digits -> {
      case parse_binary_to_decimal(binary_digits) {
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

/// Parse binary string to decimal string
fn parse_binary_to_decimal(binary_str: String) -> Result(String, Nil) {
  case int.base_parse(binary_str, 2) {
    Ok(value) -> Ok(int.to_string(value))
    Error(_) -> Error(Nil)
  }
}

fn ternary(cond: String, then: String, els: String) -> String {
  "(" <> cond <> " ? " <> then <> " : " <> els <> ")"
}

fn string_lit(val: String) {
  // for now ive made size -1 check the size at runtime
  // let size = int.to_string(string.byte_size(val))
  let val = string.replace(val, "\n", "\\n")
  let size = "-1"
  "new_String(\"" <> val <> "\", " <> size <> ")"
}

fn gen_value(arg: Value, target: String) {
  case arg {
    Literal(_, val) ->
      case val {
        core.NilVal -> hit_target(target, "0")
        core.Bool(val) -> hit_target(target, val)
        Int(val) -> hit_target(target, convert_integer_literal(val))
        Float(val) -> hit_target(target, convert_float_literal(val))
        String(val) -> hit_target(target, string_lit(val))
        BitArray(size) -> {
          let value = "new_bit_array(" <> size <> ")"
          hit_target(target, value)
        }
      }

    Variable(_, val) -> hit_target(target, escape_if_keyword(val))
  }
}

fn pointer(t: CustomType) {
  case t.variants {
    [_] -> "struct " <> t.name <> "*"
    _ -> {
      let tags =
        t.variants
        |> list.map(fn(v) { v.name <> "_TAG" })
        |> string.join(", ")
      let ptrs =
        t.variants
        |> list.map(fn(v) {
          "struct " <> v.name <> " *" <> v.display_name <> ";\n"
        })
        |> string.concat()
      "struct {\nenum {" <> tags <> "} tag;\nunion {\n" <> ptrs <> "} val; }"
    }
  }
}

/// Generate field access code
fn generate_field_access(
  kind: mono.FieldAccessKind,
  variant: String,
  field: String,
  base_expr: String,
) -> String {
  let field = escape_if_keyword(field)
  let accessor = case kind {
    mono.StructPointerAccess -> "->"
    mono.StructAccess -> "."
    mono.TaggedUnionAccess -> ".val." <> variant <> "->"
  }
  base_expr <> accessor <> field
}

/// Generate variant check code
fn generate_variant_check(
  custom: CustomType,
  variant_name: String,
  base_expr: String,
) -> String {
  case custom.variants {
    [_] -> "True"
    _ -> base_expr <> ".tag == " <> variant_name <> "_TAG"
  }
}

/// Handle Let binding generation
fn handle_let_binding(
  c: Context,
  var: String,
  val: Term,
  exp: Term,
  target: String,
) -> String {
  let escaped_var = escape_if_keyword(var)

  case var, val {
    "_" <> _, _ -> gen_term(c, val, var) <> gen_term(c, exp, target)
    _, Value(_, val) ->
      type_name(val.typ)
      <> " "
      <> escaped_var
      <> " = "
      <> gen_value(val, "")
      <> ";\n"
      <> gen_term(c, exp, target)
    _, Call(..) ->
      type_name(val.typ)
      <> " "
      <> escaped_var
      <> " = "
      <> gen_term(c, val, "")
      <> ";\n"
      <> gen_term(c, exp, target)
    _, _ ->
      type_name(val.typ)
      <> " "
      <> escaped_var
      <> ";\n"
      <> gen_term(c, val, escaped_var)
      <> gen_term(c, exp, target)
  }
}

fn gen_term(c: Context, arg: Term, target: String) -> String {
  // TODO is "id" used?
  case arg {
    Value(_, value) -> gen_value(value, target)
    Call(_, fun, args) -> {
      let call_expr =
        gen_value(fun, "")
        <> "("
        <> string.join(list.map(args, gen_value(_, "")), ", ")
        <> ")"
      hit_target(target, call_expr)
    }
    CallClosure(typ, fun, args) -> {
      let closure = gen_value(fun, "")
      let param_types = list.map(args, fn(x) { type_name(x.typ) })
      let fun_ptr = closure <> ".fun"
      let env_param = closure <> ".env"
      let params = list.map(args, gen_value(_, ""))
      let cond = "is_closure(" <> closure <> ")"

      let type_sig = "((" <> type_name(typ) <> "(*)("
      let exp_env =
        type_sig
        <> string.join(["void*", ..param_types], ", ")
        <> "))"
        <> fun_ptr
        <> ")("
        <> string.join([env_param, ..params], ", ")
        <> ")"
      let exp_no_env =
        type_sig
        <> string.join(param_types, ", ")
        <> "))"
        <> fun_ptr
        <> ")("
        <> string.join(params, ", ")
        <> ")"

      hit_target(target, ternary(cond, exp_env, exp_no_env))
    }
    Op(_, op, args) -> {
      case op, args {
        mono.FieldAccess(kind, v, f), [arg] -> {
          generate_field_access(kind, v, f, gen_value(arg, ""))
          |> hit_target(target, _)
        }
        mono.VariantCheck(v), [arg] -> {
          // get details from the type definition
          let name = type_name(arg.typ)
          let assert Ok(custom) = dict.get(c.types, name)
          let assert Ok(variant) =
            list.find(custom.variants, fn(x) { x.untyped_name == v })

          generate_variant_check(custom, variant.name, gen_value(arg, ""))
          |> hit_target(target, _)
        }
        mono.Panic, [arg] -> "panic_exit(" <> gen_value(arg, "") <> ");\n"
        _, _ -> {
          panic as "invalid operation"
        }
      }
    }
    Let(_, var, val, exp) -> handle_let_binding(c, var, val, exp, target)
    If(_, cond, then_exp, else_exp) ->
      "if ("
      <> gen_value(cond, "")
      <> ") {\n"
      <> gen_term(c, then_exp, target)
      <> "} else {\n"
      <> gen_term(c, else_exp, target)
      <> "}\n"
  }
}

/// Generate function signature (common between function and function_forward)
fn generate_function_signature(
  name: String,
  params: List(closure.Field),
  return: mono.Type,
) -> String {
  let param_list =
    list.map(params, fn(p) {
      type_name(p.typ) <> " " <> escape_if_keyword(p.name)
    })
    |> string.join(", ")

  type_name(return) <> " " <> name <> "(" <> param_list <> ")"
}

fn function(c: Context, fun: Function) -> String {
  let body = normalise.normalise_exp(fun.body, 0)
  generate_function_signature(fun.name, fun.parameters, fun.return)
  <> " {\n"
  <> gen_term(c, body, "RETURN")
  <> "}"
}

pub fn function_forward(fun: Function) -> String {
  generate_function_signature(fun.name, fun.parameters, fun.return) <> ";"
}

fn custom_type_def(t: CustomType) {
  case t.pointer {
    True -> "typedef " <> pointer(t) <> " " <> t.name <> ";"
    False ->
      case t.variants {
        [v] -> "typedef struct " <> v.name <> " " <> t.name <> ";"
        _ -> panic as "unexpected struct type with multiple variants"
      }
  }
}

fn custom_type_forward(t: CustomType) {
  let equal =
    "Bool eq_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b" <> ");"

  let less_than =
    "Bool lt_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b" <> ");"

  let inspect = "String inspect_" <> t.name <> "(" <> t.name <> " a);"

  let constructors =
    t.variants
    |> list.map(constructor_function_header(t, _))
    |> list.map(fn(con) { con <> ";" })

  [equal, less_than, inspect, ..constructors]
  |> string.join("\n")
}

fn unwrap_pointer(ptr: String, v: closure.Variant) {
  case v.fields {
    [] -> ""
    _ ->
      "struct "
      <> v.name
      <> " s"
      <> ptr
      <> " = *"
      <> ptr
      <> ".val."
      <> v.display_name
      <> ";\n"
  }
}

fn struct_literal(v: closure.Variant) {
  let fields_str =
    v.fields
    |> list.map(fn(f) {
      let field_name = escape_if_keyword(f.name)
      "." <> field_name <> " = " <> field_name
    })
    |> string.join(",")

  "((struct " <> v.name <> "){" <> fields_str <> "})"
}

/// Generate field comparison for equality functions
fn generate_field_equality_check(
  field_typ: mono.Type,
  field_name: String,
  access_op: String,
) -> String {
  let escaped_name = escape_if_keyword(field_name)
  "if(!eq_"
  <> type_name(field_typ)
  <> "(a"
  <> access_op
  <> escaped_name
  <> ", b"
  <> access_op
  <> escaped_name
  <> ")) { return False; }\n"
}

/// Generate field comparison for less-than functions
fn generate_field_less_than_check(
  field_typ: mono.Type,
  field_name: String,
  access_op: String,
) -> String {
  let escaped_name = escape_if_keyword(field_name)
  let field_lt = "lt_" <> type_name(field_typ)
  "if("
  <> field_lt
  <> "(a"
  <> access_op
  <> escaped_name
  <> ", b"
  <> access_op
  <> escaped_name
  <> ")) { return True; }\nif("
  <> field_lt
  <> "(b"
  <> access_op
  <> escaped_name
  <> ", a"
  <> access_op
  <> escaped_name
  <> ")) { return False; }\n"
}

/// Generate equality function for custom type
fn generate_equality_function(t: CustomType) -> String {
  let access_op = case t.pointer {
    True -> "->"
    False -> "."
  }

  let function_header =
    "Bool eq_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b) {\n"

  let body = case t.variants {
    [v] ->
      list.map(v.fields, fn(f) {
        generate_field_equality_check(f.typ, f.name, access_op)
      })
      |> string.concat
    variants -> {
      let variant_checks =
        variants
        |> list.map(fn(v) {
          let field_checks =
            v.fields
            |> list.map(fn(f) {
              let field_equal = "eq_" <> type_name(f.typ)
              let escaped_field_name = escape_if_keyword(f.name)
              "if(!"
              <> field_equal
              <> "(sa."
              <> escaped_field_name
              <> ", sb."
              <> escaped_field_name
              <> ")) { return False; }\n"
            })
            |> string.concat
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v)
          <> unwrap_pointer("b", v)
          <> field_checks
          <> "}\n"
        })
        |> string.join("else ")
      "if (a.tag != b.tag) { return False; }\n" <> variant_checks
    }
  }

  function_header <> body <> "return True;\n}\n"
}

/// Generate less-than function for custom type
fn generate_less_than_function(t: CustomType) -> String {
  let access_op = case t.pointer {
    True -> "->"
    False -> "."
  }

  let function_header =
    "Bool lt_" <> t.name <> "(" <> t.name <> " a, " <> t.name <> " b) {\n"

  let body = case t.variants {
    [v] ->
      list.map(v.fields, fn(f) {
        generate_field_less_than_check(f.typ, f.name, access_op)
      })
      |> string.concat
    variants -> {
      let variant_checks =
        variants
        |> list.map(fn(v) {
          let field_checks =
            v.fields
            |> list.map(fn(f) {
              let field_lt = "lt_" <> type_name(f.typ)
              let field_name = escape_if_keyword(f.name)
              "if("
              <> field_lt
              <> "(sa."
              <> field_name
              <> ", sb."
              <> field_name
              <> ")) { return True; }\nif("
              <> field_lt
              <> "(sb."
              <> field_name
              <> ", sa."
              <> field_name
              <> ")) { return False; }\n"
            })
            |> string.concat
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v)
          <> unwrap_pointer("b", v)
          <> field_checks
          <> "}\n"
        })
        |> string.join("else ")
      "if (a.tag < b.tag) { return True; }\nif (a.tag > b.tag) { return False; }\n"
      <> variant_checks
    }
  }

  function_header <> body <> "return False;\n}\n"
}

/// Generate inspect fields for a variant
fn generate_inspect_fields(
  fields: List(closure.Field),
  access_op: String,
  prefix: String,
) -> List(String) {
  fields
  |> list.map(fn(f) {
    "inspect_"
    <> type_name(f.typ)
    <> "("
    <> prefix
    <> access_op
    <> escape_if_keyword(f.name)
    <> ")"
  })
}

/// Generate inspect function for custom type
fn generate_inspect_function(t: CustomType) -> String {
  let access_op = case t.pointer {
    True -> "->"
    False -> "."
  }

  let function_header =
    "String inspect_" <> t.name <> "(" <> t.name <> " a) {\n"

  let body = case t.variants {
    [v] ->
      case v.fields {
        [] -> "return " <> string_lit(v.display_name) <> ";\n"
        fields -> {
          let field_inspects =
            generate_inspect_fields(fields, access_op, "a")
            |> list.intersperse(string_lit(", "))
            |> list.fold(string_lit(v.display_name <> "("), fn(a, f) {
              "append_string(\n" <> a <> ",\n" <> f <> ")"
            })
          "return append_string(\n"
          <> field_inspects
          <> ",\n"
          <> string_lit(")")
          <> ");\n"
        }
      }
    variants -> {
      let variant_cases =
        variants
        |> list.map(fn(v) {
          let variant_body = case v.fields {
            [] -> "return " <> string_lit(v.display_name) <> ";\n"
            fields -> {
              let field_inspects =
                generate_inspect_fields(fields, ".", "sa")
                |> list.intersperse(string_lit(", "))
                |> list.fold(string_lit(v.display_name <> "("), fn(a, f) {
                  "append_string(\n" <> a <> ",\n" <> f <> ")"
                })
              "return append_string(\n"
              <> field_inspects
              <> ",\n"
              <> string_lit(")")
              <> ");\n"
            }
          }
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v)
          <> variant_body
          <> "}\n"
        })
        |> string.concat
      variant_cases <> "return " <> string_lit("???") <> ";\n"
    }
  }

  function_header <> body <> "}\n"
}

/// Generate variant definitions for custom type
fn generate_variant_definitions(t: CustomType) -> String {
  list.map(t.variants, fn(v) {
    let tag = v.name <> "_TAG"
    let struct = custom_type_struct(v) <> "\n"

    let constructor = case v.fields {
      [] ->
        "const "
        <> t.name
        <> " new_"
        <> v.name
        <> " = {.tag="
        <> tag
        <> ", .val=0};\n"
      _ -> {
        let header = constructor_function_header(t, v)
        let body = case t.pointer {
          True -> {
            let malloc_part =
              "struct "
              <> v.name
              <> " *_ptr = malloc(sizeof(struct "
              <> v.name
              <> "));\n"
            let assign_part = "*_ptr = " <> struct_literal(v) <> ";\n"
            let return_part = case t.variants {
              [_] -> "return _ptr;"
              _ ->
                "return (("
                <> t.name
                <> "){.tag="
                <> tag
                <> ", .val."
                <> v.display_name
                <> "=_ptr});\n"
            }
            malloc_part <> assign_part <> return_part
          }
          False -> "return " <> struct_literal(v) <> ";\n"
        }
        header <> " {\n" <> body <> "}\n"
      }
    }

    struct <> constructor
  })
  |> string.concat
}

fn custom_type(t: CustomType) {
  generate_variant_definitions(t)
  <> generate_equality_function(t)
  <> generate_less_than_function(t)
  <> generate_inspect_function(t)
}

fn constructor_function_header(t: CustomType, v: closure.Variant) -> String {
  case v.fields {
    [] -> "extern const " <> t.name <> " new_" <> v.name
    _ -> {
      let params =
        v.fields
        |> list.map(fn(p) {
          type_name(p.typ) <> " " <> escape_if_keyword(p.name)
        })
        |> string.join(", ")
      t.name <> " new_" <> v.name <> "(" <> params <> ")"
    }
  }
}

fn custom_type_struct(v: closure.Variant) {
  let fields =
    v.fields
    |> list.map(fn(f) {
      type_name(f.typ) <> " " <> escape_if_keyword(f.name) <> ";\n"
    })
    |> string.join("")
  "struct " <> v.name <> "{\n" <> fields <> "};"
}

/// Sort types by dependency (extracted common logic)
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
      True, False -> order.Gt
      False, False -> order.Eq
      False, True -> order.Lt
    }
  })
  |> list.filter(fn(t) { t.name != "Bool" && t.name != "Nil" })
}

pub fn module_header(mod: Module) -> String {
  let types = sort_types_by_dependency(mod)

  let type_decl = list.map(types, custom_type_def) |> string.join("\n")
  let type_forward = list.map(types, custom_type_forward) |> string.join("\n")
  let fun_decl = list.map(mod.externals, function_forward) |> string.join("\n")
  let type_structs =
    list.map(types, fn(t) {
      list.map(t.variants, custom_type_struct) |> string.join("\n")
    })
    |> string.join("\n\n")

  [type_decl, type_forward, fun_decl, type_structs]
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> string.join("\n\n")
}

pub fn module(mod: Module) -> String {
  let types = sort_types_by_dependency(mod)
  let types_dict = list.map(types, fn(t) { #(t.name, t) }) |> dict.from_list()
  let c = Context(types: types_dict)

  let type_def = list.map(types, custom_type_def) |> string.join("\n\n")
  let type_forward = list.map(types, custom_type_forward) |> string.join("\n\n")
  let type_impl = list.map(types, custom_type) |> string.join("\n\n")
  let ext_fun_decl =
    list.map(mod.externals, function_forward) |> string.join("\n")
  let fun_decl =
    list.map(mod.functions, function_forward) |> string.join("\n\n")
  let fun_impl = list.map(mod.functions, function(c, _)) |> string.join("\n\n")

  [type_def, type_forward, type_impl, ext_fun_decl, fun_decl, fun_impl]
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> string.join("\n\n")
}
