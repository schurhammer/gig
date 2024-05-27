import glance as g
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import graph

pub type ExpVar =
  String

pub type Module {
  Module(types: List(TypeDef), functions: List(Function))
}

pub type TypeDef {
  TypeDef(name: String, params: List(String), variants: List(VariantDef))
}

pub type VariantDef {
  VariantDef(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Type)
}

pub type Type {
  TypeVar(var: TypeVar)
  TypeApp(typ: String, args: List(Type))
  TypeFun(ret: Type, args: List(Type))
}

pub type Function {
  Function(name: ExpVar, params: List(String), body: Exp)
}

pub type Exp {
  ExpInt(val: Int)
  ExpVar(var: ExpVar)
  ExpApp(fun: Exp, args: List(Exp))
  ExpAbs(var: List(ExpVar), exp: Exp)
  ExpLet(var: ExpVar, val: Exp, exp: Exp)
  ExpIf(cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type TypeVar =
  Int

fn rename_var(replace: String, with: String, in: Exp) -> Exp {
  case in {
    ExpInt(_) -> in
    ExpVar(var) ->
      case var == replace {
        True -> ExpVar(with)
        False -> in
      }
    ExpApp(fun, args) -> {
      let fun = rename_var(replace, with, fun)
      let args = list.map(args, rename_var(replace, with, _))
      ExpApp(fun, args)
    }
    ExpAbs(vars, exp) ->
      case list.contains(vars, replace) {
        True -> in
        False -> ExpAbs(vars, rename_var(replace, with, exp))
      }
    ExpLet(var, val, exp) ->
      case var == replace {
        True -> ExpLet(var, rename_var(replace, with, val), exp)
        False ->
          ExpLet(
            var,
            rename_var(replace, with, val),
            rename_var(replace, with, exp),
          )
      }
    ExpIf(cond, then_exp, else_exp) ->
      ExpIf(
        rename_var(replace, with, cond),
        rename_var(replace, with, then_exp),
        rename_var(replace, with, else_exp),
      )
  }
}

fn unshadow(taken: List(String), i: Int, exp: Exp) {
  case exp {
    ExpInt(_) -> exp
    ExpVar(_) -> exp
    ExpApp(fun, args) -> {
      let fun = unshadow(taken, i, fun)
      let args = list.map(args, unshadow(taken, i, _))
      ExpApp(fun, args)
    }
    ExpAbs(vars, exp) -> {
      let #(taken, i, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var) {
            True -> {
              let new_var = "V" <> int.to_string(i) <> "_" <> var
              let i = i + 1
              let exp = rename_var(var, new_var, exp)
              let var = new_var
              let taken = [var, ..taken]
              let exp = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
            False -> {
              let taken = [var, ..taken]
              let exp = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
          }
        })
      let exp = unshadow(taken, i, exp)
      let vars = list.reverse(vars)
      ExpAbs(vars, exp)
    }
    ExpLet(var, val, exp) ->
      case list.contains(taken, var) {
        True -> {
          let val = unshadow(taken, i, val)
          let new_var = "V" <> int.to_string(i) <> "_" <> var
          let i = i + 1
          let exp = rename_var(var, new_var, exp)
          let var = new_var
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          ExpLet(var, val, exp)
        }
        False -> {
          let val = unshadow(taken, i, val)
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          ExpLet(var, val, exp)
        }
      }
    ExpIf(cond, then_exp, else_exp) -> {
      let cond = unshadow(taken, i, cond)
      let then_exp = unshadow(taken, i, then_exp)
      let else_exp = unshadow(taken, i, else_exp)
      ExpIf(cond, then_exp, else_exp)
    }
  }
}

pub fn unshadow_module(module: Module) -> Module {
  let fun_names = list.map(module.functions, fn(fun) { fun.name })
  let functions =
    list.map(module.functions, fn(fun) {
      Function(fun.name, fun.params, unshadow(fun_names, 1, fun.body))
    })
  let types = module.types
  Module(types, functions)
}

pub fn call_graph(
  g: graph.Graph(String),
  from: String,
  e: Exp,
) -> graph.Graph(String) {
  case e {
    ExpInt(_) -> g
    ExpVar(to) -> graph.insert_edge(g, from, to)
    ExpApp(fun, args) -> {
      let g = call_graph(g, from, fun)
      list.fold(args, g, fn(g, arg) { call_graph(g, from, arg) })
    }
    ExpAbs(_, exp) -> call_graph(g, from, exp)
    ExpLet(_, val, exp) -> {
      let g = call_graph(g, from, val)
      call_graph(g, from, exp)
    }
    ExpIf(cond, then_e, else_e) -> {
      let g = call_graph(g, from, cond)
      let g = call_graph(g, from, then_e)
      call_graph(g, from, else_e)
    }
  }
}

pub fn module_to_core(mod: g.Module) {
  let funs = list.map(mod.functions, fn(fun) { function_to_core(fun) })
  Module(types: [], functions: funs)
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
  Function(fun.name, params, body)
}

const panic_exp = ExpApp(ExpVar("panic"), [])

fn block_to_core(block: List(g.Statement)) -> Exp {
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
        g.Expression(e) -> ExpLet("_", expression_to_core(e), block_to_core(xs))
      }
  }
}

fn pattern_check_to_core(pattern: g.Pattern, subject: Exp) -> Exp {
  case pattern {
    g.PatternInt(value) -> {
      let value = int_to_core(value)
      ExpApp(ExpVar("equal"), [value, subject])
    }
    g.PatternVariable(_) -> ExpVar("True")

    // g.PatternConstructor(_, name, _, _) -> {
    // }
    _ -> {
      io.debug(pattern)
      io.debug(subject)
      todo
    }
  }
}

fn pattern_bind_to_core(pattern: g.Pattern, subject: Exp, exp: Exp) -> Exp {
  case pattern {
    g.PatternInt(_) -> exp
    g.PatternVariable(x) -> ExpLet(x, subject, exp)
    _ -> todo
  }
}

fn int_to_core(value: String) {
  let assert Ok(n) = int.parse(value)
  ExpInt(n)
}

fn expression_to_core(e: g.Expression) -> Exp {
  case e {
    g.Int(n) -> int_to_core(n)
    g.Variable(s) -> ExpVar(s)
    g.BinaryOperator(name, left, right) ->
      case name {
        g.Eq ->
          ExpApp(ExpVar("equal"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.AddInt ->
          ExpApp(ExpVar("add_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.SubInt ->
          ExpApp(ExpVar("sub_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.MultInt ->
          ExpApp(ExpVar("mul_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        g.DivInt ->
          ExpApp(ExpVar("div_int"), [
            expression_to_core(left),
            expression_to_core(right),
          ])
        _ -> todo
      }
    g.Call(function, arguments) ->
      ExpApp(
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
              let subject = ExpVar("S" <> int.to_string(i))
              pattern_check_to_core(pattern, subject)
            })
            |> list.reduce(fn(a, b) { ExpApp(ExpVar("and_bool"), [a, b]) })
            |> result.unwrap(ExpVar("False"))
          let then_exp =
            list.index_fold(patterns, expression_to_core(body), fn(exp, pat, i) {
              let subject = ExpVar("S" <> int.to_string(i))
              pattern_bind_to_core(pat, subject, exp)
            })

          ExpIf(cond, then_exp, else_exp)
        })
      list.index_fold(subjects, exp, fn(exp, sub, i) {
        ExpLet("S" <> int.to_string(i), expression_to_core(sub), exp)
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
      ExpAbs(params, body)
    }
    _ -> {
      io.println_error("Not Implemented:")
      io.debug(e)
      todo
    }
  }
}
