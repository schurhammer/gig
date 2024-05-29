import graph

import glance as g

import gleam/io
import gleam/list

type Graph =
  graph.Graph(String)

type Env =
  List(String)

pub fn create(module: g.Module) {
  let g =
    list.fold(module.functions, graph.new(), fn(g, f) {
      graph.insert_node(g, f.definition.name)
    })

  list.fold(module.functions, g, fn(g, f) { walk_function(g, [], f.definition) })
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
        g.Use(..) -> todo
        g.Assignment(_, pattern, _, e) -> {
          let p = pattern_bindings(pattern)
          let n = combine_env(p, n)
          let g = walk_expression(g, n, r, e)
          walk_body(g, n, r, xs)
        }
        g.Expression(e) -> {
          let g = walk_expression(g, n, r, e)
          walk_body(g, n, r, xs)
        }
      }
  }
}

fn pattern_bindings(pattern: g.Pattern) -> List(String) {
  case pattern {
    g.PatternInt(_) -> []
    g.PatternVariable(x) -> [x]
    _ -> {
      io.debug(pattern)
      todo
    }
  }
}

fn walk_expression(g: Graph, n: Env, r: String, e: g.Expression) -> Graph {
  case e {
    g.Int(_) -> g
    g.Variable(s) ->
      //  it's a function reference if the variable is not in the local env
      case list.contains(n, s) {
        False -> graph.insert_edge(g, r, s)
        True -> g
      }
    g.Call(fun, args) -> {
      let g = walk_expression(g, n, r, fun)
      list.fold(args, g, fn(g, e) { walk_expression(g, n, r, e.item) })
    }
    g.BinaryOperator(..) -> g
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
    _ -> {
      io.debug(e)
      todo
    }
  }
}
