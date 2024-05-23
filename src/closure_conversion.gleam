import core.{
  type Exp, type Function, ExpAbs, ExpApp, ExpIf, ExpInt, ExpLet, ExpVar,
  Function,
}

import gleam/list

// TODO move to typed expressions? need to know the types of stuff to put in closure

// assumptions:
// 1. renaming has been done such that there is no shadowing of values

type CC {
  CC(funs: List(Function), uid: Int)
}

fn cc(c: CC, s: List(#(String)), e: Exp) -> #(CC, Exp) {
  case e {
    ExpInt(_) -> #(c, e)
    ExpVar(_) -> #(c, e)
    ExpApp(fun, args) -> {
      let #(c, fun) = cc(c, s, fun)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, s, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, ExpApp(fun, args))
    }
    ExpAbs(vars, exp) -> {
      let #(c, exp) = cc(c, s, e)
      #(c, exp)
    }
    ExpLet(var, val, exp) -> {
      let #(c, exp) = cc(c, s, e)
      #(c, exp)
    }
    ExpIf(cond, then_e, else_e) -> #(c, e)
  }
}
