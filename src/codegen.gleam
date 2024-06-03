import closure_conversion.{
  type Exp, type Function, type Module, Call, CallClosure, If, Int, Let, Module,
  Var,
}
import monomorphise.{type CustomType, type Mono, CustomType, MonoApp, MonoFun} as mono

import graph
import type_graph

import gleam/bool
import gleam/int
import gleam/list
import gleam/string

// TODO standardise type name functions
fn type_name(typ: Mono) -> String {
  "T_" <> do_type_name(typ)
}

fn do_type_name(typ: Mono) -> String {
  case typ {
    MonoApp(name, args) ->
      string.join([name, ..list.map(args, do_type_name)], "_")
    MonoFun(..) -> {
      // TODO what do
      "Closure"
    }
  }
}

fn hit_target(target: String, with: String) {
  case with {
    // TODO find a better solution for panic
    "panic()" -> "panic();\n"
    _ ->
      case target {
        "" -> with
        // TODO not sure if this is always valid
        // "RETURN" -> "return " <> with <> ";\n"
        target -> target <> " = " <> with <> ";\n"
      }
  }
}

fn ternary(cond: String, then: String, els: String) -> String {
  "(" <> cond <> " ? " <> then <> " : " <> els <> ")"
}

fn texp(arg: Exp, target: String, id: Int) -> String {
  case arg {
    Int(_, val) -> hit_target(target, val)

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
      let cond = env_param

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
  <> type_name(ret)
  <> " RETURN;\n"
  <> texp(body, "RETURN", 1)
  <> "return RETURN;\n"
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
    "T_Bool"
    <> " equal_"
    <> t.name
    <> "("
    <> "T_"
    <> t.name
    <> " a, "
    <> "T_"
    <> t.name
    <> " b"
    <> ");\n"
  let variants =
    list.map(t.variants, fn(v) {
      // TODO we don't really need a typedef for the struct
      // it's only used in the union
      "typedef struct "
      <> case t.pointer {
        True -> v.name <> "*"
        False -> v.name
      }
      <> " S_"
      <> v.name
      <> ";\n"
    })
    |> string.concat
  let union = case t.variants {
    // only a single variant
    [v] -> "typedef S_" <> v.name <> " T_" <> t.name <> ";\n"
    // either 0 or many variants
    variants -> {
      let union_name = "U_" <> t.name
      let union =
        "struct "
        <> union_name
        <> " {\nint tag;\n"
        <> "union {\n"
        <> list.map(variants, fn(v) { "S_" <> v.name <> " " <> v.name <> ";\n" })
        |> string.concat()
        <> "}\n"
        <> "data;\n"
        <> "};\n"
      let typedef = "typedef struct " <> union_name <> " T_" <> t.name <> ";\n"
      union <> typedef
    }
  }
  variants <> union <> equal
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
    "T_Bool"
    <> " equal_"
    <> t.name
    <> "("
    <> "T_"
    <> t.name
    <> " a, "
    <> "T_"
    <> t.name
    <> " b"
    <> ") {\n"
    <> case t.variants {
      [variant] ->
        list.map(variant.fields, fn(f) {
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
      variants ->
        "if (a.tag != b.tag) { return False; }\n"
        <> list.index_map(variants, fn(v, i) {
          "if (a.tag == "
          <> int.to_string(i)
          <> ") {\n"
          <> list.map(v.fields, fn(f) {
            let field_equal = "equal" <> mono.type_name(f.typ)
            "if(!"
            <> field_equal
            <> "(a.data."
            <> v.name
            <> access_op
            <> f.name
            <> ", b.data."
            <> v.name
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

  let variant_definitions =
    list.index_map(t.variants, fn(v, i) {
      let tag = int.to_string(i)
      let fields =
        list.map(v.fields, fn(f) { type_name(f.typ) <> " " <> f.name <> ";\n" })
        |> string.join("")
      let struct = "struct " <> v.name <> "{\n" <> fields <> "};\n"
      let constructor_target = case is_record {
        True -> "RETURN"
        False -> "RETURN.data." <> v.name
      }
      let constructor =
        "T_"
        <> t.name
        <> " "
        <> v.name
        <> "("
        <> v.fields
        |> list.map(fn(p) { type_name(p.typ) <> " " <> p.name })
        |> string.join(", ")
        <> ") {\n"
        <> "T_"
        <> t.name
        <> " RETURN;\n"
        <> case is_record {
          True -> ""
          False -> "RETURN.tag = " <> tag <> ";\n"
        }
        <> case t.pointer {
          True ->
            case v.fields {
              [] -> constructor_target <> " = " <> "(void*) 1;\n"
              _ ->
                constructor_target
                <> " = "
                <> "malloc(sizeof(struct "
                <> v.name
                <> "));\n"
            }
          _ -> ""
        }
        <> v.fields
        |> list.map(fn(p) {
          constructor_target <> access_op <> p.name <> " = " <> p.name <> ";\n"
        })
        |> string.join("")
        <> "return RETURN;\n"
        <> "}\n"
      let instanceof =
        "T_Bool"
        <> " "
        <> v.name
        <> "_instanceof"
        <> "("
        <> "T_"
        <> t.name
        <> " data) { return "
        <> case is_record {
          True -> "True"
          False -> "data.tag == " <> tag
        }
        <> "; }\n"
      let getters =
        list.map(v.fields, fn(f) {
          type_name(f.typ)
          <> " "
          <> v.name
          <> "_"
          <> f.name
          <> "("
          <> "T_"
          <> t.name
          <> " data) { return "
          <> case is_record {
            True -> "data" <> access_op

            False -> "data.data." <> v.name <> access_op
          }
          <> f.name
          <> "; }\n"
        })
      [struct, constructor, instanceof, ..getters]
      |> string.concat
    })
    |> string.concat

  variant_definitions <> equal
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
