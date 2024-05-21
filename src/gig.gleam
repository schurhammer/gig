import glance as g
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import lc as c

fn module_to_core(mod: g.Module) {
  let funs = list.map(mod.functions, fn(fun) { function_to_core(fun) })
  c.Module(functions: funs)
}

fn function_to_core(def: g.Definition(g.Function)) {
  //   Function(
  //     name: String,
  //     publicity: Publicity,
  //     parameters: List(FunctionParameter),
  //     return: Option(Type),
  //     body: List(Statement),
  //     location: Span,
  //   )
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
  c.Function(fun.name, c.ExpAbs(params, body))
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
        g.Assignment(pattern: p, value: e, ..) -> todo
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

fn pattern_bind_to_core(pattern: g.Pattern, exp: c.Exp) -> c.Exp {
  todo
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
        |> list.reverse
        |> list.fold(panic_exp, fn(else_exp, clause) {
          let cond =
            list.map(clause.patterns, fn(patterns) {
              list.index_map(patterns, fn(pattern, i) {
                let subject = c.ExpVar("SUBJECT" <> int.to_string(i))
                pattern_check_to_core(pattern, subject)
              })
              |> list.reduce(fn(a, b) { c.ExpApp(c.ExpVar("and_bool"), [a, b]) })
              |> result.unwrap(c.ExpVar("False"))
            })
            |> list.reduce(fn(a, b) { c.ExpApp(c.ExpVar("or_bool"), [a, b]) })
            |> result.unwrap(c.ExpVar("False"))
          let then_exp = expression_to_core(clause.body)
          c.ExpIf(cond, then_exp, else_exp)
        })
      // TODO bind pattern
      list.index_fold(subjects, exp, fn(exp, sub, i) {
        c.ExpLet("SUBJECT" <> int.to_string(i), expression_to_core(sub), exp)
      })
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

const prelude = [
  #("panic", c.Poly(1, c.Mono(c.TypeApp("->", [c.TypeVar(1)])))),
  #(
    "equal",
    c.Poly(1, c.Mono(c.TypeApp("->", [bool, c.TypeVar(1), c.TypeVar(1)]))),
  ),
  // bool
  #("True", c.Mono(bool)), #("False", c.Mono(bool)),
  #("and_bool", c.Mono(c.TypeApp("->", [bool, bool, bool]))),
  #("or_bool", c.Mono(c.TypeApp("->", [bool, bool, bool]))),
  // int
  #("add_int", c.Mono(c.TypeApp("->", [int, int, int]))),
  #("sub_int", c.Mono(c.TypeApp("->", [int, int, int]))),
  #("mul_int", c.Mono(c.TypeApp("->", [int, int, int]))),
  #("div_int", c.Mono(c.TypeApp("->", [int, int, int]))),
]

pub fn main() {
  let assert Ok(module) =
    g.module(
      "
      fn fact(n) {
        case n {
          0 -> 1
          n -> n * fact(n - 1)
        }
      }
  ",
    )
  let core = module_to_core(module)
  io.debug(core)
  io.println_error("\n")
  list.each(core.functions, fn(fun) {
    fun.body
    |> c.pretty_print_exp
    |> io.println_error()
  })
  io.println_error("\n")
  let assert Ok(res) = c.w_module(dict.from_list(prelude), core)
  list.each(res.functions, fn(fun) {
    fun.body
    |> c.pretty_print_texp
    |> io.println_error()
  })
  Nil
}
