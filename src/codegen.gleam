import closure_conversion.{
  type Exp, type Function, type Module, Call, CallClosure, Function, If, Int,
  Let, Module, Var,
}
import core.{type Type, type TypeDef, TypeApp, TypeDef, TypeFun, TypeVar}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

fn type_name(typ: Type) -> String {
  case typ {
    TypeVar(_) -> panic as "type vars should be resolved"
    TypeApp(name, args) -> string.join([name, ..list.map(args, type_name)], "_")
    TypeFun(..) -> {
      // TODO what do
      "Closure"
    }
  }
}

fn hit_target(target: String, with: String) {
  case target {
    "" -> with
    // TODO not sure if this is always valid
    // "RETURN" -> "return " <> with <> ";\n"
    target -> target <> " = " <> with <> ";\n"
  }
}

fn texp(arg: Exp, target: String, id: Int) -> String {
  case arg {
    Int(_, val) -> hit_target(target, int.to_string(val))

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
      let cast =
        "("
        <> type_name(typ)
        <> "(*)"
        <> "("
        <> string.join(["void*", ..param_types], ", ")
        <> ")"
        <> ")"
      let fun = closure <> ".fun"
      let env_param = closure <> ".env"
      let params = list.map(args, texp(_, "", id))

      let exp =
        "("
        <> cast
        <> fun
        <> ")"
        <> "("
        <> string.join([env_param, ..params], ", ")
        <> ")"

      hit_target(target, exp)
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
  let assert TypeFun(ret, param_types) = fun.typ
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
  let assert TypeFun(ret, param_types) = fun.typ
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

fn type_def_forward(t: TypeDef) {
  list.map(t.variants, fn(v) {
    "typedef struct " <> v.name <> " " <> v.name <> ";"
  })
  |> string.join("\n")
}

fn type_def(t: TypeDef) {
  // TODO handle union types
  list.map(t.variants, fn(v) {
    let fields =
      list.map(v.fields, fn(f) { type_name(f.typ) <> " " <> f.name <> ";\n" })
      |> string.join("")

    let struct = "struct " <> v.name <> "{\n" <> fields <> "};"

    let constructor =
      v.name
      <> "* "
      <> v.name
      <> "_NEW"
      <> "("
      <> v.fields
      |> list.map(fn(p) { type_name(p.typ) <> " " <> p.name })
      |> string.join(", ")
      <> ") {\n"
      <> v.name
      <> "* RETURN = malloc(sizeof("
      <> v.name
      <> "));\n"
      <> v.fields
      |> list.map(fn(p) { "RETURN->" <> p.name <> " = " <> p.name <> ";\n" })
      |> string.join("")
      <> "return RETURN;\n"
      <> "}"
    let getters =
      list.map(v.fields, fn(f) {
        type_name(f.typ)
        <> " "
        <> v.name
        <> "_GET_"
        <> f.name
        <> "("
        <> v.name
        <> "* data) { return data->"
        <> f.name
        <> "; }"
      })
    [struct, constructor, ..getters]
    |> string.join("\n\n")
  })
  |> string.join("\n")
}

pub fn module(mod: Module) -> String {
  let funs = mod.functions
  let types = mod.types

  let type_decl =
    list.map(types, type_def_forward)
    |> string.join("\n\n")

  let type_impl =
    list.map(types, type_def)
    |> string.join("\n\n")

  let fun_decl =
    list.map(funs, function_forward)
    |> string.join("\n\n")

  let fun_impl =
    list.map(funs, function)
    |> string.join("\n\n")

  [type_decl, type_impl, fun_decl, fun_impl]
  |> string.join("\n\n")
}
