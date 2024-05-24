import gleam/list

/// A Directed Graph Structure.
pub opaque type Graph(a) {
  Graph(adj: List(#(a, List(a))))
}

/// Create an empty graph.
pub fn new() -> Graph(a) {
  Graph([])
}

/// Insert the node into the graph.
pub fn insert_node(g: Graph(a), node: a) -> Graph(a) {
  Graph(adj: [#(node, []), ..g.adj])
}

/// Insert a directed edge into the graph.
pub fn insert_edge(g: Graph(a), from: a, to: a) -> Graph(a) {
  Graph(
    list.map(g.adj, fn(x) {
      case x.0 == from {
        True -> #(x.0, [to, ..x.1])
        False -> x
      }
    }),
  )
}

pub fn neighbours(g: Graph(a), from: a) -> List(a) {
  case list.find(g.adj, fn(x) { x.0 == from }) {
    Ok(n) -> n.1
    Error(_) -> []
  }
}

// Reverse edges of the graph
fn reverse_graph(g: Graph(a)) -> Graph(a) {
  g.adj
  |> list.map(fn(x) { #(x.0, []) })
  |> Graph()
  |> list.fold(
    g.adj,
    _,
    fn(g, i) {
      let to = i.0
      list.fold(i.1, g, fn(g, from) { insert_edge(g, from, to) })
    },
  )
}

pub fn kosaraju(g: Graph(a)) {
  let #(_, l) =
    list.fold(g.adj, #([], []), fn(acc, i) {
      let #(v, l) = acc
      visit(g, v, l, i.0)
    })

  let r = reverse_graph(g)
  let a = list.fold(r.adj, [], fn(a, i) { assign(r, a, i.0, i.0) })
  a
}

fn visit(g: Graph(a), v: List(a), l: List(a), u: a) -> #(List(a), List(a)) {
  // If u is unvisited then:
  //   Mark u as visited.
  //   For each out-neighbour v of u, do Visit(v).
  //   Prepend u to L.
  // Otherwise do nothing.
  case list.contains(v, u) {
    False -> {
      let v = [u, ..v]
      let #(v, l) =
        list.fold(neighbours(g, u), #(v, l), fn(acc, i) {
          let #(v, l) = acc
          visit(g, v, l, i)
        })
      let l = [u, ..l]
      #(v, l)
    }
    True -> #(v, l)
  }
}

/// g should be the reversed graph
/// a is assignments
fn assign(g: Graph(a), a: List(#(a, a)), u: a, root: a) -> List(#(a, a)) {
  case has_assignment(a, u) {
    False -> {
      let a = [#(u, root), ..a]
      list.fold(neighbours(g, u), a, fn(a, i) { assign(g, a, i, root) })
    }
    True -> a
  }
}

fn has_assignment(a: List(#(a, a)), u: a) -> Bool {
  case list.find(a, fn(x) { x.0 == u }) {
    Ok(_) -> True
    Error(_) -> False
  }
}
