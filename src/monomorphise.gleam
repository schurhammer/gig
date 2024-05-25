import core.{
  type Env, type Poly, type TExp, type TFunction, type TModule, type Type, Mono,
  Poly, TExpAbs, TExpApp, TExpIf, TExpInt, TExpLet, TExpVar, TFunction, TModule,
  TypeApp, TypeFun, TypeVar,
}

import gleam/dict
import gleam/io
import gleam/list
import gleam/string

const type_tag_seperator = "_T_"

type MM {
  MM(poly: TModule, mono: TModule)
}

pub fn run(poly: TModule) -> TModule {
  let assert Ok(main) = list.find(poly.functions, fn(x) { x.name == "main" })

  let m = MM(poly, TModule([]))
  let assert Mono(typ) = main.typ
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

fn unify_poly(poly: Poly, mono: Type) -> #(List(Type), List(#(Int, Type))) {
  case poly {
    Mono(x) -> #([], unify_type(x, mono))
    Poly(v, x) -> {
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

fn instantiate_fun(fun: TFunction, typ: Type) -> TFunction {
  let subs = unify_poly(fun.typ, typ)

  let mono_name =
    fun.name
    <> type_tag_seperator
    <> subs.0
    |> list.map(type_to_string)
    |> string.join("_")

  let sub = dict.from_list(subs.1)

  let mono_body = core.apply_sub_texpr(sub, fun.body1)

  TFunction(mono_name, fun.params, mono_body, Mono(typ))
}

fn instantiate(m: MM, fun_name: String, typ: Type) -> #(MM, String) {
  case list.find(m.poly.functions, fn(x) { x.name == fun_name }) {
    Ok(fun) -> {
      let inst = instantiate_fun(fun, typ)
      case list.find(m.mono.functions, fn(x) { x.name == inst.name }) {
        // already instantiated
        Ok(_) -> #(m, inst.name)
        // add instantiation
        _ -> {
          let #(m, exp) = mm(m, inst.body1)
          let inst = TFunction(inst.name, inst.params, exp, inst.typ)
          let funs = [inst, ..m.mono.functions]
          let mono = TModule(funs)
          let m = MM(..m, mono: mono)
          #(m, inst.name)
        }
      }
    }
    Error(_) -> #(m, fun_name)
  }
}

fn mm(m: MM, e: TExp) -> #(MM, TExp) {
  case e {
    TExpInt(_, _) -> #(m, e)
    TExpVar(typ, var) -> {
      let #(m, name) = instantiate(m, var, typ)
      io.debug(name)
      #(m, TExpVar(typ, name))
    }
    TExpApp(typ, fun, args) -> {
      let #(m, fun) = mm(m, fun)
      let #(m, args) =
        list.fold(args, #(m, []), fn(acc, arg) {
          let #(m, args) = acc
          let #(m, arg) = mm(m, arg)
          #(m, [arg, ..args])
        })
      let args = list.reverse(args)
      #(m, TExpApp(typ, fun, args))
    }
    TExpAbs(typ, vars, exp) -> {
      let #(m, exp) = mm(m, exp)
      #(m, TExpAbs(typ, vars, exp))
    }
    TExpLet(typ, var, val, exp) -> {
      let #(m, val) = mm(m, val)
      let #(m, exp) = mm(m, exp)
      #(m, TExpLet(typ, var, val, exp))
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let #(m, cond) = mm(m, cond)
      let #(m, then_e) = mm(m, then_e)
      let #(m, else_e) = mm(m, else_e)
      #(m, TExpIf(typ, cond, then_e, else_e))
    }
  }
}
