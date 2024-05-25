import gleam/io
import gleeunit/should
import graph.{insert_edge, insert_node, new, strongly_connected_components}

pub fn conencted_components_test() {
  // Test case 1: Acyclic graph
  let graph =
    new()
    |> insert_node(1)
    |> insert_node(2)
    |> insert_node(3)
    |> insert_node(4)
    |> insert_node(5)
    |> insert_edge(5, 2)
    |> insert_edge(4, 2)
    |> insert_edge(4, 3)
    |> insert_edge(3, 1)

  let expected = [[2], [5], [1], [3], [4]]
  strongly_connected_components(graph) |> should.equal(expected)

  // Test case 2: Graph with mutually recursive nodes
  let graph =
    new()
    |> insert_node(1)
    |> insert_node(2)
    |> insert_node(3)
    |> insert_node(4)
    |> insert_node(5)
    |> insert_edge(1, 2)
    |> insert_edge(2, 1)
    |> insert_edge(2, 3)
    |> insert_edge(3, 4)
    |> insert_edge(4, 5)

  let expected = [[5], [4], [3], [1, 2]]
  strongly_connected_components(graph) |> should.equal(expected)

  // Test case 3: Graph with multiple mutually recursive groups
  let graph =
    new()
    |> insert_node(1)
    |> insert_node(2)
    |> insert_node(3)
    |> insert_node(4)
    |> insert_node(5)
    |> insert_node(6)
    |> insert_edge(1, 2)
    |> insert_edge(2, 1)
    |> insert_edge(3, 4)
    |> insert_edge(4, 3)
    |> insert_edge(5, 6)

  let expected = [[6], [5], [3, 4], [1, 2]]
  strongly_connected_components(graph) |> should.equal(expected)

  // Test case 4: Empty graph
  let graph = new()
  let expected = []
  strongly_connected_components(graph)
  |> should.equal(expected)
}
