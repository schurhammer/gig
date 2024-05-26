import core as c
import glance as g
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub fn module_to_core(mod: g.Module) {
  let funs = list.map(mod.functions, fn(fun) { function_to_core(fun) })
  c.Module(types: [], functions: funs)
}

fn function_to_core(def: g.Definition(g.Function)) {
  let fun = def.definition
  let params =
    fun.parameters
    |> list.map(fn(param) {
      case param.name {
        g.Named(n) -> n
        _ -> "_"
      }
    })

  let body = block_to_core(fun.body)
  c.Function(fun.name, params, body)
}

const panic_exp = c.ExpApp(c.ExpVar("panic"), [])

fn block_to_core(block: List(g.Statement)) -> c.Exp {
  case block {
    [] -> panic_exp
    [x] ->
      case x {
        g.Use(..) -> todo
        // if the assignment is in the last position we can just ignore the pattern
        g.Assignment(value: e, ..) -> expression_to_core(e)
        g.Expression(e) -> expression_to_core(e)
      }
    [x, ..xs] ->
      case x {
        g.Use(..) -> todo
        g.Assignment(pattern: p, value: e, ..) ->
          pattern_bind_to_core(p, expression_to_core(e), block_to_core(xs))
        g.Expression(e) ->
          c.ExpLet("_", expression_to_core(e), block_to_core(xs))
      }
  }
}

fn pattern_check_to_core(pattern: g.Pattern, subject: c.Exp) -> c.Exp {
  case pattern {
    g.PatternInt(value) -> {
      let value = int_to_core(value)
      c.ExpApp(c.ExpVar("equal"), [value, subject])
    }
    g.PatternVariable(_) -> c.ExpVar("True")
    _ -> todo
  }
}

fn pattern_bind_to_core(pattern: g.Pattern, subject: c.Exp, exp: c.Exp) -> c.Exp {
  case pattern {
    g.PatternInt(_) -> exp
    g.PatternVariable(x) -> c.ExpLet(x, subject, exp)
    _ -> todo
  }
}

fn int_to_core(value: String) {
  let assert Ok(n) = int.parse(value)
  c.ExpInt(n)
}

fn expression_to_core(e: g.Expression) -> c.Exp {
  case e {
    g.Int(n) -> int_to_core(n)
    g.Variable(s) -> c.ExpVar(s)
    g.BinaryOperator(name, left, right) ->
      case name {
        g.Eq ->
          c.ExpApp(c.ExpVar("equal"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.AddInt ->
          c.ExpApp(c.ExpVar("add_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.SubInt ->
          c.ExpApp(c.ExpVar("sub_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.MultInt ->
          c.ExpApp(c.ExpVar("mul_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.DivInt ->
          c.ExpApp(c.ExpVar("div_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        _ -> todo
      }
    g.Call(function, arguments) ->
      c.ExpApp(
        expression_to_core(function),
        list.map(arguments, fn(x) { expression_to_core(x.item) }),
      )
    g.Case(subjects, clauses) -> {
      let exp =
        clauses
        // unwrap alternative case patterns into multiple clauses
        // we do this because they bind variables differently
        |> list.flat_map(fn(clause: g.Clause) {
          list.map(clause.patterns, fn(patterns) { #(patterns, clause.body) })
        })
        |> list.reverse
        |> list.fold(panic_exp, fn(else_exp, clause) {
          let #(patterns, body) = clause
          let cond =
            list.index_map(patterns, fn(pattern, i) {
              let subject = c.ExpVar("S" <> int.to_string(i))
              pattern_check_to_core(pattern, subject)
            })
            |> list.reduce(fn(a, b) { c.ExpApp(c.ExpVar("and_bool"), [a, b]) })
            |> result.unwrap(c.ExpVar("False"))
          let then_exp =
            list.index_fold(patterns, expression_to_core(body), fn(exp, pat, i) {
              let subject = c.ExpVar("S" <> int.to_string(i))
              pattern_bind_to_core(pat, subject, exp)
            })

          c.ExpIf(cond, then_exp, else_exp)
        })
      list.index_fold(subjects, exp, fn(exp, sub, i) {
        c.ExpLet("S" <> int.to_string(i), expression_to_core(sub), exp)
      })
    }
    g.Fn(args, ret, body) -> {
      let params =
        args
        |> list.map(fn(param) {
          case param.name {
            g.Named(n) -> n
            _ -> "_"
          }
        })
      let body = block_to_core(body)
      c.ExpAbs(params, body)
    }
    _ -> {
      io.println_error("Not Implemented:")
      io.debug(e)
      todo
    }
  }
}

const bool = c.TypeApp("Bool", [])

const int = c.TypeApp("Int", [])

pub const prelude = [
  #("panic", c.Poly(1, c.Mono(c.TypeFun(c.TypeVar(1), [])))),
  #("equal", c.Poly(1, c.Mono(c.TypeFun(bool, [c.TypeVar(1), c.TypeVar(1)])))),
  // bool
  #("True", c.Mono(bool)), #("False", c.Mono(bool)),
  #("and_bool", c.Mono(c.TypeFun(bool, [bool, bool]))),
  #("or_bool", c.Mono(c.TypeFun(bool, [bool, bool]))),
  // int
  #("add_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("sub_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("mul_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("div_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("print_int", c.Mono(c.TypeFun(int, [int]))),
]

pub fn main() {
  let assert Ok(module) =
    g.module(
      "
      fn fact(n) {
        case n {
          0 | 1 -> 1
          n -> n * fact(n - 1)
        }
      }
      fn main() {
        print_int(fact(6))
        0
      }
  ",
    )
  let core = module_to_core(module)
  io.debug(core)
  io.println_error("\n")
  list.each(core.functions, fn(fun) {
    c.ExpAbs(fun.params, fun.body)
    |> c.pretty_print_exp
    |> io.println_error()
  })
  io.println_error("\n")
  let assert Ok(module) = c.w_module(dict.from_list(prelude), core)
  Nil
}
