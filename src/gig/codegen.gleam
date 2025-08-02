import gig/closure.{type CustomType, type Function, type Module, Field}
import gig/core.{BitArray, Float, Int, String}
import gig/gen_names
import gig/mono.{type_name}
import gig/normalise.{
  type Term, type Value, Call, CallClosure, If, Let, Literal, Panic, Value,
  Variable,
}
import gig/type_graph
import gleam/order

import gig/graph

import gleam/int
import gleam/list
import gleam/string

/// List of C keywords that cannot be used as variable names or function parameters
const c_keywords = [
  "auto", "break", "case", "char", "const", "continue", "default", "do",
  "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline",
  "int", "long", "register", "restrict", "return", "short", "signed", "sizeof",
  "static", "struct", "switch", "typedef", "union", "unsigned", "void",
  "volatile", "while", "asm", "typeof", "main", "printf", "malloc", "free",
  "exit", "abort",
]

/// Check if a given name is a C keyword or reserved identifier
fn is_keyword(name: String) -> Bool {
  list.contains(c_keywords, name)
}

/// Escape a name if it's a C keyword by appending an underscore
/// If the name is not a keyword, return it unchanged
fn escape_if_keyword(name: String) -> String {
  case is_keyword(name) {
    True -> "_" <> name
    False -> name
  }
}

fn hit_target(target: String, with: String) {
  case target {
    // no target means it is part of an expression
    "" -> with
    // discarded, no need to set the target
    "_" <> _ -> with <> ";\n"
    // TODO this causes warnings about functions not having return
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
  // TODO this doesnt work for strings like "\n"
  // for now ive made size -1 check the size at runtime
  // let size = int.to_string(string.byte_size(val))
  let val = string.replace(val, "\n", "\\n")
  let size = "-1"
  "new_String(\"" <> val <> "\", " <> size <> ")"
}

fn gen_value(arg: Value, target: String, id: Int) {
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
    _ ->
      "struct { enum {"
      <> t.variants
      |> list.map(fn(v) { v.name <> "_TAG" })
      |> string.join(", ")
      <> "} tag; union {"
      <> t.variants
      |> list.index_map(fn(v, i) {
        "struct " <> v.name <> " *v" <> int.to_string(i) <> ";"
      })
      |> string.join(" ")
      <> "} ptr; "
      <> "}"
  }
}

fn gen_term(arg: Term, target: String, id: Int) -> String {
  // TODO is "id" used?
  case arg {
    Value(typ, value) -> gen_value(value, target, id)
    Call(typ, fun, args) -> {
      hit_target(
        target,
        gen_value(fun, "", id)
          <> "("
          <> list.map(args, gen_value(_, "", id)) |> string.join(", ")
          <> ")",
      )
    }
    CallClosure(typ, fun, args) -> {
      let closure = gen_value(fun, "", id)
      let param_types = list.map(args, fn(x) { type_name(x.typ) })

      let fun = closure <> ".fun"
      let env_param = closure <> ".env"
      let params = list.map(args, gen_value(_, "", id))

      // the closure may or may not have an env parameter
      // determined by if env is a null pointer
      let cond = "is_closure(" <> closure <> ")"

      let exp_env =
        "("
        <> "("
        <> type_name(typ)
        <> "(*)"
        <> "("
        <> string.join(["void*", ..param_types], ", ")
        <> ")"
        <> ")"
        <> fun
        <> ")"
        <> "("
        <> string.join([env_param, ..params], ", ")
        <> ")"

      let exp_no_env =
        "("
        <> "("
        <> type_name(typ)
        <> "(*)"
        <> "("
        <> string.join(param_types, ", ")
        <> ")"
        <> ")"
        <> fun
        <> ")"
        <> "("
        <> string.join(params, ", ")
        <> ")"

      hit_target(target, ternary(cond, exp_env, exp_no_env))
    }
    Panic(typ, val) -> {
      "panic_exit(" <> gen_value(val, "", id) <> ");\n"
    }
    Let(typ, var, val, exp) ->
      case var, val {
        "_" <> _, _ -> {
          // discarded, no need to make a variable
          gen_term(val, var, id) <> gen_term(exp, target, id)
        }
        _, Value(typ, val) -> {
          // inline value
          let escaped_var = escape_if_keyword(var)
          type_name(val.typ)
          <> " "
          <> escaped_var
          <> " = "
          <> gen_value(val, "", id)
          <> ";\n"
          <> gen_term(exp, target, id)
        }
        _, Call(..) -> {
          // inline call
          let escaped_var = escape_if_keyword(var)
          type_name(val.typ)
          <> " "
          <> escaped_var
          <> " = "
          <> gen_term(val, "", id)
          <> ";\n"
          <> gen_term(exp, target, id)
        }
        _, _ -> {
          // complex expression
          let escaped_var = escape_if_keyword(var)
          type_name(val.typ)
          <> " "
          <> escaped_var
          <> ";\n"
          <> gen_term(val, escaped_var, id)
          <> gen_term(exp, target, id)
        }
      }
    If(typ, cond, then_exp, else_exp) ->
      "if ("
      <> gen_value(cond, "", id)
      <> ") {\n"
      <> gen_term(then_exp, target, id + 1)
      <> "} else {\n"
      <> gen_term(else_exp, target, id + 1)
      <> "}\n"
  }
}

fn function(fun: Function) -> String {
  let params = fun.params
  let body = normalise.normalise_exp(fun.body, 0)
  let assert core.FunctionType(param_types, ret) = fun.typ
  type_name(ret)
  <> " "
  <> fun.name
  <> "("
  <> list.zip(params, param_types)
  |> list.map(fn(p) {
    let #(name, typ) = p
    type_name(typ) <> " " <> escape_if_keyword(name)
  })
  |> string.join(", ")
  <> ") {\n"
  <> gen_term(body, "RETURN", 1)
  <> "}"
}

pub fn function_forward(fun: Function) -> String {
  // io.debug(fun)
  let params = fun.params
  let assert core.FunctionType(param_types, ret) = fun.typ
  type_name(ret)
  <> " "
  <> fun.name
  <> "("
  <> list.zip(params, param_types)
  |> list.map(fn(p) {
    let #(name, typ) = p
    type_name(typ) <> " " <> escape_if_keyword(name)
  })
  |> string.join(", ")
  <> ");"
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

fn unwrap_pointer(ptr: String, v: closure.Variant, vi: Int) {
  case v.fields {
    [] -> ""
    _ ->
      "struct "
      <> v.name
      <> " s"
      <> ptr
      <> " = *"
      <> ptr
      <> ".ptr.v"
      <> int.to_string(vi)
      <> ";\n"
  }
}

fn struct_literal(v: closure.Variant) {
  { "((struct " <> v.name <> "){" }
  <> {
    v.fields
    |> list.map(fn(f) {
      let field_name = escape_if_keyword(f.name)
      "." <> field_name <> " = " <> field_name
    })
    |> string.join(",")
  }
  <> "})"
}

fn custom_type(t: CustomType) {
  // TODO align naming conventions with rest of code
  // TODO handle union types
  let is_record = case t.variants {
    [_] -> True
    _ -> False
  }
  let access_op = case t.pointer {
    True -> "->"
    False -> "."
  }

  let equal =
    "Bool"
    <> " eq_"
    <> t.name
    <> "("
    <> t.name
    <> " a, "
    <> t.name
    <> " b"
    <> ") {\n"
    <> case t.variants {
      [v] ->
        list.map(v.fields, fn(f) {
          let field_name = escape_if_keyword(f.name)
          "if(!"
          <> "eq_"
          <> type_name(f.typ)
          <> "(a"
          <> access_op
          <> field_name
          <> ", b"
          <> access_op
          <> field_name
          <> ")) { return False; }\n"
        })
        |> string.concat
      variants ->
        "if (a.tag != b.tag) { return False; }\n"
        <> list.index_map(variants, fn(v, vi) {
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v, vi)
          <> unwrap_pointer("b", v, vi)
          <> list.map(v.fields, fn(f) {
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
          <> "}\n"
        })
        |> string.join("else ")
    }
    <> "return True;\n"
    <> "}\n"

  let less_than =
    "Bool lt_"
    <> t.name
    <> "("
    <> t.name
    <> " a, "
    <> t.name
    <> " b"
    <> ") {\n"
    <> case t.variants {
      [v] ->
        list.map(v.fields, fn(f) {
          let field_lt = "lt_" <> type_name(f.typ)
          let field_name = escape_if_keyword(f.name)
          "if("
          <> field_lt
          <> "(a"
          <> access_op
          <> field_name
          <> ", b"
          <> access_op
          <> field_name
          <> ")) { return True; }\n"
          <> "if("
          <> field_lt
          <> "(b"
          <> access_op
          <> field_name
          <> ", a"
          <> access_op
          <> field_name
          <> ")) { return False; }\n"
        })
        |> string.concat
      variants ->
        "if (a.tag < b.tag) { return True; }\n"
        <> "if (a.tag > b.tag) { return False; }\n"
        <> list.index_map(variants, fn(v, vi) {
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v, vi)
          <> unwrap_pointer("b", v, vi)
          <> list.map(v.fields, fn(f) {
            let field_lt = "lt_" <> type_name(f.typ)
            let field_name = escape_if_keyword(f.name)
            "if("
            <> field_lt
            <> "(sa."
            <> field_name
            <> ", sb."
            <> field_name
            <> ")) { return True; }\n"
            <> "if("
            <> field_lt
            <> "(sb."
            <> field_name
            <> ", sa."
            <> field_name
            <> ")) { return False; }\n"
          })
          |> string.concat
          <> "}\n"
        })
        |> string.join("else ")
    }
    <> "return False;\n"
    <> "}\n"

  let inspect =
    "String inspect_"
    <> t.name
    <> "("
    <> t.name
    <> " a) {\n"
    <> case t.variants {
      [v] ->
        case v.fields {
          [] -> "return " <> string_lit(v.display_name) <> ";\n"
          _ ->
            "return append_string("
            <> v.fields
            |> list.map(fn(f) {
              "inspect_"
              <> type_name(f.typ)
              <> "(a"
              <> access_op
              <> escape_if_keyword(f.name)
              <> ")"
            })
            |> list.intersperse(string_lit(", "))
            |> list.fold(string_lit(v.display_name <> "("), fn(a, f) {
              "append_string(" <> a <> ", " <> f <> ")"
            })
            <> ", "
            <> string_lit(")")
            <> ");\n"
        }
      variants ->
        list.index_map(variants, fn(v, vi) {
          "if (a.tag == "
          <> v.name
          <> "_TAG) {\n"
          <> unwrap_pointer("a", v, vi)
          <> case v.fields {
            [] -> "return " <> string_lit(v.display_name) <> ";\n"
            _ ->
              "return append_string("
              <> v.fields
              |> list.map(fn(f) {
                "inspect_"
                <> type_name(f.typ)
                <> "(sa."
                <> escape_if_keyword(f.name)
                <> ")"
              })
              |> list.intersperse(string_lit(", "))
              |> list.fold(string_lit(v.display_name <> "("), fn(a, f) {
                "append_string(" <> a <> ", " <> f <> ")"
              })
              <> ", "
              <> string_lit(")")
              <> ");\n"
          }
          <> "}\n"
        })
        |> string.concat
    }
    <> "return "
    <> string_lit("???")
    <> ";\n"
    <> "}\n"

  let variant_definitions =
    list.index_map(t.variants, fn(v, vi) {
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
          <> ", .ptr=0};\n"
        _ ->
          constructor_function_header(t, v)
          <> " {\n"
          <> case t.pointer {
            True ->
              "struct "
              <> v.name
              <> " *_ptr = malloc(sizeof(struct "
              <> v.name
              <> "));\n"
              <> "*_ptr = "
              <> struct_literal(v)
              <> ";\n"
              <> case t.variants {
                [_] -> "return _ptr;"
                _ ->
                  "return (("
                  <> t.name
                  <> "){.tag="
                  <> tag
                  <> ", .ptr.v"
                  <> int.to_string(vi)
                  <> "=_ptr});\n"
              }
            False -> "return " <> struct_literal(v) <> ";\n"
          }
          <> "}\n"
      }

      let isa =
        "Bool "
        <> gen_names.get_variant_check_name(v.name)
        <> "("
        <> t.name
        <> " a) {\n"
        <> case is_record {
          True -> "return True;\n"
          False -> "return a.tag == " <> tag <> ";\n"
        }
        <> "}\n"
      let getters =
        list.index_map(v.fields, fn(f, fi) {
          let field_name = escape_if_keyword(f.name)
          type_name(f.typ)
          <> " "
          <> gen_names.get_getter_name(v.name, fi)
          <> "("
          <> t.name
          <> " value) {"
          <> case t.pointer, t.variants {
            True, [_] -> "return value->" <> field_name
            False, _ -> "return value." <> field_name
            True, _ ->
              " struct "
              <> v.name
              <> "* ptr = value.ptr.v"
              <> int.to_string(vi)
              <> "; return ptr"
              <> access_op
              <> field_name
          }
          <> "; }\n"
        })
      [struct, constructor, isa, ..getters]
      |> string.concat
    })
    |> string.concat

  variant_definitions <> equal <> less_than <> inspect
}

fn constructor_function_header(t: CustomType, v: closure.Variant) -> String {
  case v.fields {
    [] -> "extern const " <> t.name <> " new_" <> v.name
    _ ->
      t.name
      <> " new_"
      <> v.name
      <> "("
      <> v.fields
      |> list.map(fn(p) { type_name(p.typ) <> " " <> escape_if_keyword(p.name) })
      |> string.join(", ")
      <> ")"
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

pub fn module_header(mod: Module) -> String {
  // sort types to put them in order of dependency and
  //  "pointer types" after "struct types"
  let type_graph = type_graph.create(mod)
  let type_groups = graph.strongly_connected_components(type_graph)

  let types =
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

  let type_decl =
    list.map(types, custom_type_def)
    |> string.join("\n")

  let type_forward =
    list.map(types, custom_type_forward)
    |> string.join("\n")

  let fun_decl =
    list.map(mod.externals, function_forward)
    |> string.join("\n")

  let type_structs =
    list.map(types, fn(t) {
      list.map(t.variants, custom_type_struct)
      |> string.join("\n")
    })
    |> string.join("\n\n")

  [type_decl, type_forward, fun_decl, type_structs]
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> string.join("\n\n")
}

pub fn module(mod: Module) -> String {
  // sort types to put them in order of dependency and
  //  "pointer types" after "struct types"
  let type_groups =
    type_graph.create(mod)
    |> graph.strongly_connected_components()

  let types =
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

  let type_def =
    list.map(types, custom_type_def)
    |> string.join("\n\n")

  let type_forward =
    list.map(types, custom_type_forward)
    |> string.join("\n\n")

  let type_impl =
    list.map(types, custom_type)
    |> string.join("\n\n")

  let ext_fun_decl =
    list.map(mod.externals, function_forward)
    |> string.join("\n")

  let fun_decl =
    list.map(mod.functions, function_forward)
    |> string.join("\n\n")

  let fun_impl =
    list.map(mod.functions, function)
    |> string.join("\n\n")

  [type_def, type_forward, type_impl, ext_fun_decl, fun_decl, fun_impl]
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> string.join("\n\n")
}
