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

/// Insert a directed edge into the graph. Ignores nodes not in the graph.
pub fn insert_edge(g: Graph(a), from: a, to: a) -> Graph(a) {
  let adj = case list.find(g.adj, fn(x) { x.0 == to }) {
    Ok(_) ->
      list.map(g.adj, fn(x) {
        case x.0 == from {
          True -> #(x.0, [to, ..x.1])
          False -> x
        }
      })
    // TODO panic here?
    Error(_) -> g.adj
  }
  Graph(adj)
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
  |> list.fold(g.adj, _, fn(g, i) {
    let to = i.0
    list.fold(i.1, g, fn(g, from) { insert_edge(g, from, to) })
  })
}

pub fn strongly_connected_components(g: Graph(a)) -> List(List(a)) {
  // Kosaraju's algorithm
  let #(_, l) =
    list.fold(g.adj, #([], []), fn(acc, i) {
      let #(v, l) = acc
      visit(g, v, l, i.0)
    })

  let r = reverse_graph(g)
  list.fold(l, [], fn(a, i) { assign(r, a, i, i) })
  |> list.chunk(fn(i) { i.1 })
  |> list.map(list.map(_, fn(x: #(a, a)) { x.0 }))
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

fn assign(g: Graph(a), a: List(#(a, a)), u: a, root: a) -> List(#(a, a)) {
  // If u has not been assigned to a component then:
  //   Assign u as belonging to the component whose root is root.
  //   For each in-neighbour v of u, do Assign(v,root).
  // Otherwise do nothing.
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
