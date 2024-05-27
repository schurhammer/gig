import core.{type Type, TypeApp, TypeFun, TypeVar}

import typed as t

import gleam/dict
import gleam/io
import gleam/list
import gleam/string

const type_tag_seperator = "_T_"

type MM {
  MM(poly: t.Module, mono: t.Module, done: List(String))
}

pub fn run(poly: t.Module) -> t.Module {
  let assert Ok(main) = list.find(poly.functions, fn(x) { x.name == "main" })

  let m = MM(poly, t.Module(poly.types, []), [])
  let assert t.Mono(typ) = main.typ
  let #(m, name) = instantiate(m, "main", typ)

  m.mono
}

fn unify_type(poly: Type, mono: Type) -> List(#(Int, Type)) {
  case poly, mono {
    TypeVar(av), _ -> [#(av, mono)]
    TypeApp(_, aa), TypeApp(_, ba) ->
      list.zip(aa, ba)
      |> list.flat_map(fn(x) { unify_type(x.0, x.1) })
    TypeFun(ar, aa), TypeFun(br, ba) ->
      list.append(
        unify_type(ar, br),
        list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(x.0, x.1) }),
      )

    _, _ -> {
      io.debug(poly)
      io.debug(mono)
      panic
    }
  }
}

fn unify_poly(poly: t.Poly, mono: Type) -> #(List(Type), List(#(Int, Type))) {
  case poly {
    t.Mono(x) -> #([], unify_type(x, mono))
    t.Poly(v, x) -> {
      let #(l, m) = unify_poly(x, mono)
      let t = case list.find(m, fn(x) { x.0 == v }) {
        Ok(x) -> x.1
        _ -> panic
      }
      #([t, ..l], m)
    }
  }
}

fn type_to_string(typ: Type) -> String {
  case typ {
    TypeVar(var) -> "VAR"
    TypeApp(t, args) ->
      t <> "_" <> list.map(args, type_to_string) |> string.join("_")
    TypeFun(ret, args) ->
      "fn_"
      <> type_to_string(ret)
      <> "_"
      <> list.map(args, type_to_string) |> string.join("_")
  }
}

fn instantiate_name(fun: t.Function, typ: Type) -> String {
  let subs = unify_poly(fun.typ, typ)

  fun.name
  <> type_tag_seperator
  <> subs.0
  |> list.map(type_to_string)
  |> string.join("_")
}

fn instantiate_fun(fun: t.Function, typ: Type, mono_name: String) -> t.Function {
  let subs = unify_poly(fun.typ, typ)
  let sub = dict.from_list(subs.1)
  let mono_body = t.apply_sub_texpr(sub, fun.body)
  t.Function(mono_name, fun.params, mono_body, t.Mono(typ))
}

fn instantiate(m: MM, fun_name: String, typ: Type) -> #(MM, String) {
  case list.find(m.poly.functions, fn(x) { x.name == fun_name }) {
    Ok(fun) -> {
      let inst_name = instantiate_name(fun, typ)
      case list.contains(m.done, inst_name) {
        // already instantiated
        True -> #(m, inst_name)
        // add instantiation
        False -> {
          let inst = instantiate_fun(fun, typ, inst_name)
          // must add function to context before the recursive call
          let m = MM(..m, done: [inst_name, ..m.done])
          let #(m, exp) = mm(m, inst.body)
          let inst = t.Function(inst.name, inst.params, exp, inst.typ)
          let funs = [inst, ..m.mono.functions]
          let mono = t.Module(m.mono.types, funs)
          let m = MM(..m, mono: mono)
          #(m, inst.name)
        }
      }
    }
    Error(_) -> #(m, fun_name)
  }
}

fn mm(m: MM, e: t.Exp) -> #(MM, t.Exp) {
  case e {
    t.Int(_, _) -> #(m, e)
    t.Var(typ, var) -> {
      let #(m, name) = instantiate(m, var, typ)
      #(m, t.Var(typ, name))
    }
    t.App(typ, fun, args) -> {
      let #(m, fun) = mm(m, fun)
      let #(m, args) =
        list.fold(args, #(m, []), fn(acc, arg) {
          let #(m, args) = acc
          let #(m, arg) = mm(m, arg)
          #(m, [arg, ..args])
        })
      let args = list.reverse(args)
      #(m, t.App(typ, fun, args))
    }
    t.Abs(typ, vars, exp) -> {
      let #(m, exp) = mm(m, exp)
      #(m, t.Abs(typ, vars, exp))
    }
    t.Let(typ, var, val, exp) -> {
      let #(m, val) = mm(m, val)
      let #(m, exp) = mm(m, exp)
      #(m, t.Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(m, cond) = mm(m, cond)
      let #(m, then_e) = mm(m, then_e)
      let #(m, else_e) = mm(m, else_e)
      #(m, t.If(typ, cond, then_e, else_e))
    }
  }
}
