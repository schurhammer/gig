import closure_conversion.{
  type Exp, type Function, type Module, Call, CallClosure, If, Let, Literal,
  Module, Var,
}
import monomorphise.{type CustomType, type Mono, CustomType, MonoApp, MonoFun} as mono
import typed.{Float, Int, String}

import graph
import type_graph

import gleam/bool
import gleam/int
import gleam/list
import gleam/string

fn type_name(typ: Mono) -> String {
  case typ {
    MonoApp(name, args) -> string.join([name, ..list.map(args, type_name)], "_")
    MonoFun(..) -> {
      "Closure"
    }
  }
}

fn hit_target(target: String, with: String) {
  case with {
    "panic()" -> "panic();\n"
    _ ->
      case target {
        "" -> with
        // TODO not sure if this is always valid
        // TODO could do something similar with "ignored" targets? e.g. _xyz
        "RETURN" -> "return " <> with <> ";\n"
        target -> target <> " = " <> with <> ";\n"
      }
  }
}

fn ternary(cond: String, then: String, els: String) -> String {
  "(" <> cond <> " ? " <> then <> " : " <> els <> ")"
}

fn string_lit(val: String) {
  // TODO this doesnt work for strings like "\n"
  // for now ive made size -1 check the size at runtime
  // let size = int.to_string(string.byte_size(val))
  let size = "-1"
  "String_NEW(\"" <> val <> "\", " <> size <> ")"
}

fn texp(arg: Exp, target: String, id: Int) -> String {
  case arg {
    Literal(_, val) ->
      case val {
        Int(val) -> hit_target(target, val)
        Float(val) -> hit_target(target, val <> "L")
        String(val) -> hit_target(target, string_lit(val))
      }

    Var(_, val) -> hit_target(target, val)
    Call(typ, fun, args) -> {
      hit_target(
        target,
        texp(fun, "", id)
          <> "("
          <> list.map(args, texp(_, "", id)) |> string.join(", ")
          <> ")",
      )
    }
    CallClosure(typ, fun, args) -> {
      let closure = texp(fun, "", id)
      let param_types = list.map(args, fn(x) { type_name(x.typ) })

      let fun = closure <> ".fun"
      let env_param = closure <> ".env"
      let params = list.map(args, texp(_, "", id))

      // the closure may or may not have an env parameter
      // determined by if env is a null pointer
      let cond = "is_closure(" <> closure <> ")"

      let exp_env =
        "("
        <> "("
        <> type_name(typ)
        <> "(*)"
        <> "("
        <> string.join(["Pointer", ..param_types], ", ")
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
    Let(typ, var, val, exp) ->
      type_name(val.typ)
      <> " "
      <> var
      <> ";\n"
      <> texp(val, var, id)
      <> texp(exp, target, id)
    If(typ, cond, then_exp, else_exp) ->
      "if ("
      <> texp(cond, "", id)
      <> ") {\n"
      <> texp(then_exp, target, id + 1)
      <> "} else {\n"
      <> texp(else_exp, target, id + 1)
      <> "}\n"
  }
}

fn function(fun: Function) -> String {
  let params = fun.params
  let body = fun.body
  let assert MonoFun(ret, param_types) = fun.typ
  type_name(ret)
  <> " "
  <> fun.name
  <> "("
  <> list.zip(params, param_types)
  |> list.map(fn(p) {
    let #(name, typ) = p
    type_name(typ) <> " " <> name
  })
  |> string.join(", ")
  <> ") {\n"
  <> texp(body, "RETURN", 1)
  <> "}"
}

fn function_forward(fun: Function) -> String {
  let params = fun.params
  let assert MonoFun(ret, param_types) = fun.typ
  type_name(ret)
  <> " "
  <> fun.name
  <> "("
  <> list.zip(params, param_types)
  |> list.map(fn(p) {
    let #(name, typ) = p
    type_name(typ) <> " " <> name
  })
  |> string.join(", ")
  <> ");"
}

fn custom_type_forward(t: CustomType) {
  let equal =
    "Bool"
    <> " equal_"
    <> t.name
    <> "("
    <> t.name
    <> " a, "
    <> t.name
    <> " b"
    <> ");\n"
  let typedef = case t.pointer {
    True -> "typedef Pointer " <> t.name <> ";\n"
    False ->
      case t.variants {
        [v] -> "typedef struct " <> v.name <> " " <> t.name <> ";\n"
        _ -> panic as "unexpected struct type with multiple variants"
      }
  }
  let inspect = "String inspect_" <> t.name <> "(" <> t.name <> " a);\n"
  typedef <> equal <> inspect
}

fn decode_arg(t: CustomType, var: String) -> String {
  case t.pointer {
    True ->
      "uint16_t "
      <> var
      <> "_tag = decode_tag("
      <> var
      <> ");\n"
      <> "void* "
      <> var
      <> "_ptr = decode_pointer("
      <> var
      <> ");\n"
    False -> ""
  }
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
    <> " equal_"
    <> t.name
    <> "("
    <> t.name
    <> " a, "
    <> t.name
    <> " b"
    <> ") {\n"
    <> decode_arg(t, "a")
    <> decode_arg(t, "b")
    <> case t.variants {
      [v] ->
        list.map(v.fields, fn(f) {
          let field_equal = "equal" <> mono.type_name(f.typ)
          case t.pointer {
            True ->
              "{ struct "
              <> v.name
              <> "* a = a_ptr;\n"
              <> "struct "
              <> v.name
              <> "* b = b_ptr;\n"
              <> "if(!"
              <> field_equal
              <> "(a"
              <> access_op
              <> f.name
              <> ", b"
              <> access_op
              <> f.name
              <> ")) { return False; }\n"
              <> "}"
            False ->
              "if(!"
              <> field_equal
              <> "(a"
              <> access_op
              <> f.name
              <> ", b"
              <> access_op
              <> f.name
              <> ")) { return False; }\n"
          }
        })
        |> string.concat
      variants ->
        "if (a_tag != b_tag) { return False; }\n"
        <> list.index_map(variants, fn(v, i) {
          "if (a_tag == "
          <> int.to_string(i)
          <> ") {\n"
          <> "struct "
          <> v.name
          <> "* a = a_ptr;\n"
          <> "struct "
          <> v.name
          <> "* b = b_ptr;\n"
          <> list.map(v.fields, fn(f) {
            let field_equal = "equal" <> mono.type_name(f.typ)
            "if(!"
            <> field_equal
            <> "(a"
            <> access_op
            <> f.name
            <> ", b"
            <> access_op
            <> f.name
            <> ")) { return False; }\n"
          })
          |> string.concat
          <> "}\n"
        })
        |> string.join("else ")
    }
    <> "return True;\n"
    <> "}\n"

  let inspect =
    "String inspect_"
    <> t.name
    <> "("
    <> t.name
    <> " a) {\n"
    <> decode_arg(t, "a")
    <> case t.variants {
      [v] ->
        case t.pointer {
          True -> "{ struct " <> v.name <> "* a = a_ptr;\n"
          False -> ""
        }
        <> v.fields
        |> list.fold(string_lit(v.name), fn(a, f) {
          let f =
            "inspect_" <> type_name(f.typ) <> "(a" <> access_op <> f.name <> ")"
          "append_String(" <> a <> ", " <> f <> ")"
        })
        <> ";\n"
        <> case t.pointer {
          True -> "}\n"
          False -> ""
        }
      variants ->
        list.index_map(variants, fn(v, i) {
          "if (a_tag == "
          <> int.to_string(i)
          <> ") {\n"
          <> "struct "
          <> v.name
          <> "* a = a_ptr;\n"
          <> "append_String("
          <> v.fields
          |> list.map(fn(f) {
            "inspect_" <> type_name(f.typ) <> "(a" <> access_op <> f.name <> ")"
          })
          |> list.intersperse(string_lit(", "))
          |> list.fold(string_lit(v.name <> "("), fn(a, f) {
            "append_String(" <> a <> ", " <> f <> ")"
          })
          <> ", "
          <> string_lit(")")
          <> ");\n}\n"
        })
        |> string.concat
    }
    <> "}\n"

  let variant_definitions =
    list.index_map(t.variants, fn(v, i) {
      let tag = int.to_string(i)

      let fields =
        list.map(v.fields, fn(f) { type_name(f.typ) <> " " <> f.name <> ";\n" })
        |> string.join("")

      let struct = "struct " <> v.name <> "{\n" <> fields <> "};\n"

      let constructor =
        t.name
        <> " "
        <> v.name
        <> "_NEW"
        <> "("
        <> v.fields
        |> list.map(fn(p) { type_name(p.typ) <> " " <> p.name })
        |> string.join(", ")
        <> ") {\n"
        <> case t.pointer {
          True ->
            "struct "
            <> v.name
            <> "* _ptr = "
            <> case v.fields {
              [] -> "0;\n"
              _ -> "malloc(sizeof(struct " <> v.name <> "));\n"
            }
            <> "uint16_t _tag = "
            <> tag
            <> ";\n"
          _ -> t.name <> " _value;\n"
        }
        <> v.fields
        |> list.map(fn(p) {
          case t.pointer {
            True -> "_ptr"
            False -> "_value"
          }
          <> access_op
          <> p.name
          <> " = "
          <> p.name
          <> ";\n"
        })
        |> string.join("")
        <> case t.pointer {
          True -> "return encode_pointer(_ptr, _tag);\n"
          False -> "return _value;\n"
        }
        <> "}\n"
      let isa =
        "Bool "
        <> v.name
        <> "_IS"
        <> "("
        <> t.name
        <> " a) {\n"
        <> case is_record {
          True -> "return True;\n"
          False -> decode_arg(t, "a") <> "return a_tag == " <> tag <> ";\n"
        }
        <> "}\n"
      let getters =
        list.map(v.fields, fn(f) {
          // TODO getters probably broken for record types
          type_name(f.typ)
          <> " "
          <> v.name
          <> "_"
          <> f.name
          <> "("
          <> t.name
          <> " value) {"
          <> case t.pointer {
            True ->
              " struct "
              <> v.name
              <> "* ptr = decode_pointer(value); return ptr"
              <> access_op
              <> f.name
            False -> "return value" <> access_op <> f.name
          }
          <> "; }\n"
        })
      [struct, constructor, isa, ..getters]
      |> string.concat
    })
    |> string.concat

  variant_definitions <> equal <> inspect
}

pub fn module(mod: Module) -> String {
  let funs = mod.functions

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
    |> list.sort(fn(x, y) { bool.compare(x.pointer, y.pointer) })

  let type_decl =
    list.map(types, custom_type_forward)
    |> string.join("\n\n")

  let type_impl =
    list.map(types, custom_type)
    |> string.join("\n\n")

  let fun_decl =
    list.map(funs, function_forward)
    |> string.join("\n\n")

  let fun_impl =
    list.map(funs, function)
    |> string.join("\n\n")

  [type_decl, type_impl, fun_decl, fun_impl]
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> string.join("\n\n")
}
