import core.{
  type TExp, type TFunction, TExpAbs, TExpApp, TExpIf, TExpInt, TExpLet, TExpVar,
  TFunction,
}

import gleam/list

const tag = "_T_"

type MM {
  MM(funs: List(TFunction))
}

fn mm(m: MM, s: List(#(String)), e: TExp) -> #(MM, TExp) {
  case e {
    TExpInt(_, _) -> #(m, e)
    TExpVar(_, _) -> #(m, e)
    TExpApp(typ, fun, args) -> {
      let #(m, fun) = mm(m, s, fun)
      let #(m, args) =
        list.fold(args, #(m, []), fn(acc, arg) {
          let #(m, args) = acc
          let #(m, arg) = mm(m, s, arg)
          #(m, [arg, ..args])
        })
      let args = list.reverse(args)
      #(m, TExpApp(typ, fun, args))
    }
    TExpAbs(typ, vars, exp) -> {
      let #(m, exp) = mm(m, s, e)
      #(m, exp)
    }
    TExpLet(typ, var, val, exp) -> {
      let #(m, exp) = mm(m, s, e)
      #(m, exp)
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let #(m, cond) = mm(m, s, cond)
      let #(m, then_e) = mm(m, s, then_e)
      let #(m, else_e) = mm(m, s, else_e)
      #(m, TExpIf(typ, cond, then_e, else_e))
    }
  }
}
