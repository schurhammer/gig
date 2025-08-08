import gig/closure.{type CustomType, type Field, type Module, type Variant}
import gig/graph
import gig/mono.{type_name}
import gleam/list

type Graph =
  graph.Graph(String)

pub fn create(module: Module) {
  let g =
    list.fold(module.types, graph.new(), fn(g, t) {
      graph.insert_node(g, t.name)
    })

  list.fold(module.types, g, fn(g, t) { walk_custom_type(g, t) })
}

fn walk_custom_type(g: Graph, t: CustomType) -> Graph {
  list.fold(t.variants, g, fn(g, v) { walk_varaint(g, t, v) })
}

fn walk_varaint(g: Graph, t: CustomType, v: Variant) -> Graph {
  list.fold(v.fields, g, fn(g, f) { walk_field(g, t, f) })
}

fn walk_field(g: Graph, t: CustomType, f: Field) -> Graph {
  walk_type(g, t, f.typ)
}

fn walk_type(g: Graph, t: CustomType, f: mono.Type) -> Graph {
  case f {
    mono.NamedType(_, args) -> {
      let name = type_name(f)
      let g = graph.insert_edge(g, t.name, name)
      list.fold(args, g, fn(g, arg) { walk_type(g, t, arg) })
    }
    mono.FunctionType(args, ret) -> {
      let g = walk_type(g, t, ret)
      list.fold(args, g, fn(g, arg) { walk_type(g, t, arg) })
    }
  }
}
