import closure_conversion.{
  type Exp, type Function, type Module, Call, CallClosure, If, Int, Let, Module,
  Var,
}
import gleam/int
import gleam/list
import gleam/string
import monomorphise.{type CustomType, type Mono, CustomType, MonoApp, MonoFun} as mono

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
  case target {
    "" -> with
    // TODO not sure if this is always valid
    // "RETURN" -> "return " <> with <> ";\n"
    target -> target <> " = " <> with <> ";\n"
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
  let variants =
    list.map(t.variants, fn(v) {
      "typedef struct " <> v.name <> "* T_" <> v.name <> ";\n"
    })
    |> string.concat
  let union = case t.variants {
    // only a single variant
    [v] -> "typedef T_" <> v.name <> " T_" <> t.name <> ";"
    // either 0 or many variants
    _ -> todo
  }
  variants <> union
}

fn custom_type(t: CustomType) {
  // TODO align naming conventions with rest of code
  // TODO handle union types
  list.map(t.variants, fn(v) {
    let fields =
      list.map(v.fields, fn(f) { type_name(f.typ) <> " " <> f.name <> ";\n" })
      |> string.join("")

    let struct = "struct " <> v.name <> "{\n" <> fields <> "};"

    let constructor =
      "T_"
      <> v.name
      <> " "
      <> v.name
      <> "("
      <> v.fields
      |> list.map(fn(p) { type_name(p.typ) <> " " <> p.name })
      |> string.join(", ")
      <> ") {\n"
      <> "T_"
      <> v.name
      <> " RETURN = malloc(sizeof("
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
        <> "_"
        <> f.name
        <> "("
        <> "T_"
        <> v.name
        <> " data) { return data->"
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
