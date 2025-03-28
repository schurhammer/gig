import gig/graph

import glance as g

import gleam/list
import gleam/option.{None, Some}

type Graph =
  graph.Graph(String)

type Env =
  List(String)

pub fn function_graph(module: g.Module) {
  let g =
    list.fold(module.functions, graph.new(), fn(g, f) {
      graph.insert_node(g, f.definition.name)
    })

  list.fold(module.functions, g, fn(g, f) { walk_function(g, [], f.definition) })
}

pub fn constant_graph(module: g.Module) {
  let g =
    list.fold(module.constants, graph.new(), fn(g, f) {
      graph.insert_node(g, f.definition.name)
    })

  list.fold(module.constants, g, fn(g, f) {
    walk_expression(g, [], f.definition.name, f.definition.value)
  })
}

fn combine_env(a: Env, b: Env) -> Env {
  list.fold(a, b, fn(b, i) { [i, ..b] })
}

fn walk_function(g: Graph, n: Env, fun: g.Function) -> Graph {
  let params =
    list.map(fun.parameters, fn(x) {
      case x.name {
        g.Named(x) -> x
        g.Discarded(x) -> x
      }
    })
  let n = combine_env(params, n)
  walk_body(g, n, fun.name, fun.body)
}

fn walk_body(g: Graph, n: Env, r: String, body: List(g.Statement)) -> Graph {
  case body {
    [] -> g
    [x, ..xs] ->
      case x {
        g.Use(patterns, expression) -> {
          let g = walk_expression(g, n, r, expression)
          let n =
            list.fold(patterns, n, fn(n, pattern) {
              let p = pattern_bindings(pattern)
              combine_env(p, n)
            })
          walk_body(g, n, r, xs)
        }
        g.Assignment(_, pattern, _, e) -> {
          let g = walk_expression(g, n, r, e)
          let p = pattern_bindings(pattern)
          let n = combine_env(p, n)
          walk_body(g, n, r, xs)
        }
        g.Expression(e) -> {
          let g = walk_expression(g, n, r, e)
          walk_body(g, n, r, xs)
        }
      }
  }
}

// returns a list of the names of variables that are bound
fn pattern_bindings(pattern: g.Pattern) -> List(String) {
  case pattern {
    g.PatternInt(_) -> []
    g.PatternFloat(_) -> []
    g.PatternString(_) -> []
    g.PatternDiscard(_) -> []
    g.PatternVariable(x) -> [x]
    g.PatternAssignment(pattern, var) -> [var, ..pattern_bindings(pattern)]
    g.PatternTuple(args) -> list.flat_map(args, pattern_bindings)
    g.PatternList(elements, tail) -> {
      let x = list.flat_map(elements, pattern_bindings)
      case tail {
        Some(tail) -> combine_env(x, pattern_bindings(tail))
        _ -> x
      }
    }
    g.PatternBitString(segs) ->
      list.flat_map(segs, fn(seg) { pattern_bindings(seg.0) })
    g.PatternConstructor(_mod, _cons, args, _spread) ->
      list.flat_map(args, fn(x) {
        pattern_bindings(field_item(x, g.PatternVariable))
      })
    g.PatternConcatenate(_prefix, prefix_name, rest_name) -> {
      let prefix = case prefix_name {
        Some(g.Named(name)) -> [name]
        _ -> []
      }
      let rest = case rest_name {
        g.Named(name) -> [name]
        _ -> []
      }
      combine_env(prefix, rest)
    }
  }
}

fn field_item(field: g.Field(a), constructer: fn(String) -> a) {
  case field {
    g.LabelledField(_, item) -> item
    g.ShorthandField(name) -> constructer(name)
    g.UnlabelledField(item) -> item
  }
}

fn walk_expression(g: Graph, n: Env, r: String, e: g.Expression) -> Graph {
  case e {
    g.Int(..) -> g
    g.Float(..) -> g
    g.String(..) -> g
    g.FieldAccess(subject, _) -> walk_expression(g, n, r, subject)
    g.Variable(s) ->
      //  it's a function reference if the variable is not in the local env
      case list.contains(n, s) {
        False -> graph.insert_edge(g, r, s)
        True -> g
      }
    g.NegateBool(e) -> walk_expression(g, n, r, e)
    g.NegateInt(e) -> walk_expression(g, n, r, e)
    g.Block(statements) -> walk_body(g, n, r, statements)
    g.Panic(e) ->
      case e {
        Some(e) -> walk_expression(g, n, r, e)
        None -> g
      }
    g.Todo(e) ->
      case e {
        Some(e) -> walk_expression(g, n, r, e)
        None -> g
      }
    g.Tuple(args) ->
      list.fold(args, g, fn(g, e) { walk_expression(g, n, r, e) })
    g.List(elements, rest) -> {
      let g = list.fold(elements, g, fn(g, e) { walk_expression(g, n, r, e) })
      case rest {
        Some(rest) -> walk_expression(g, n, r, rest)
        None -> g
      }
    }
    g.Call(fun, args) -> {
      let g = walk_expression(g, n, r, fun)
      list.fold(args, g, fn(g, e) {
        walk_expression(g, n, r, field_item(e, g.Variable))
      })
    }
    g.TupleIndex(tuple, _index) -> walk_expression(g, n, r, tuple)
    g.FnCapture(_label, fun, before, after) -> {
      let before = list.map(before, field_item(_, g.Variable))
      let after = list.map(after, field_item(_, g.Variable))
      let args = list.flatten([[fun], before, after])
      list.fold(args, g, fn(g, e) { walk_expression(g, n, r, e) })
    }
    g.BinaryOperator(_, left, right) -> {
      let g = walk_expression(g, n, r, left)
      walk_expression(g, n, r, right)
    }
    g.Case(subjects, clauses) -> {
      let g = list.fold(subjects, g, fn(g, e) { walk_expression(g, n, r, e) })

      let clauses =
        list.flat_map(clauses, fn(clause: g.Clause) {
          list.map(clause.patterns, fn(patterns) { #(patterns, clause.body) })
        })

      list.fold(clauses, g, fn(g, clause) {
        let #(patterns, e) = clause
        let n =
          list.fold(patterns, n, fn(n, pattern) {
            combine_env(pattern_bindings(pattern), n)
          })
        walk_expression(g, n, r, e)
      })
    }
    g.Fn(params, _, body) -> {
      let params =
        list.map(params, fn(x) {
          case x.name {
            g.Named(x) -> x
            g.Discarded(x) -> x
          }
        })
      let n = combine_env(params, n)
      walk_body(g, n, r, body)
    }
    g.RecordUpdate(_module, _constructor, record, fields) -> {
      let g = walk_expression(g, n, r, record)
      list.fold(fields, g, fn(g, f) {
        case f.item {
          Some(item) -> walk_expression(g, n, r, item)
          None -> g
        }
      })
    }
    g.BitString(segs) ->
      list.fold(segs, g, fn(g, seg) { walk_expression(g, n, r, seg.0) })
  }
}
