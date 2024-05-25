import core.{
  type TExp, type TFunction, type TModule, type Type, TExpAbs, TExpApp, TExpIf,
  TExpInt, TExpLet, TExpVar, TFunction, TModule,
}

import env
import gleam/io
import gleam/list

// TODO direct calls to top level functions could be added,
// but any higher-order usage probably need to be closure converted
// otherwise it could introduce problematic polymorphism
// especially in functions that return higher order functions

// assumptions:
// 1. renaming has been done such that there is no shadowing

type CC {
  CC(mod: TModule, uid: Int)
}

type Env =
  env.Env(String, Type)

pub fn cc_module(mod: TModule) {
  let c = CC(mod, 1)
  list.fold(mod.functions, c, fn(c, fun) {
    let #(c, e) = cc(c, fun.body1)
    c
  })
  mod
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: TExp) -> List(String) {
  case e {
    TExpInt(_, _) -> []
    TExpVar(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [var]
      }
    }
    TExpApp(typ, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    TExpAbs(typ, vars, exp) -> {
      let n = combine(vars, n)
      fv(n, exp)
    }
    TExpLet(typ, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, exp)
      combine(v, fv(n, exp))
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
  }
}

fn cc(m: CC, e: TExp) -> #(CC, TExp) {
  case e {
    TExpInt(_, _) -> #(m, e)
    TExpVar(typ, var) -> {
      #(m, TExpVar(typ, var))
    }
    TExpApp(typ, fun, args) -> {
      let #(m, fun) = cc(m, fun)
      let #(m, args) =
        list.fold(args, #(m, []), fn(acc, arg) {
          let #(m, args) = acc
          let #(m, arg) = cc(m, arg)
          #(m, [arg, ..args])
        })
      let args = list.reverse(args)
      #(m, TExpApp(typ, fun, args))
    }
    TExpAbs(typ, vars, exp) -> {
      io.debug(vars)
      let #(m, exp) = cc(m, exp)
      #(m, TExpAbs(typ, vars, exp))
    }
    TExpLet(typ, var, val, exp) -> {
      let #(m, val) = cc(m, val)
      let #(m, exp) = cc(m, exp)
      #(m, TExpLet(typ, var, val, exp))
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let #(m, cond) = cc(m, cond)
      let #(m, then_e) = cc(m, then_e)
      let #(m, else_e) = cc(m, else_e)
      #(m, TExpIf(typ, cond, then_e, else_e))
    }
  }
}
