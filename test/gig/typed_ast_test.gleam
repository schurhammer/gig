import birdie
import cymbal as y
import gig/typed_ast as t
import glance
import gleam/dict
import gleam/list

pub fn simple_test() {
  let assert Ok(parsed) =
    glance.module(
      "
      const five = 5

      pub fn add_five(a) {
        a + five
      }
      ",
    )

  let assert Ok(checked) = t.infer_module(dict.new(), parsed, "high_five")

  module_to_yaml(checked)
  |> y.encode
  |> birdie.snap(title: "simple test")
}

fn module_to_yaml(module: t.Module) -> y.Yaml {
  y.block([
    #("name", y.string(module.name)),
    #("imports", ylist(module.imports, import_to_yaml)),
    #("type_aliases", ylist(module.type_aliases, type_alias_to_yaml)),
    #("types", ylist(module.custom_types, custom_type_to_yaml)),
    #("constants", ylist(module.constants, constant_to_yaml)),
    #("functions", ylist(module.functions, function_to_yaml)),
  ])
}

fn ylist(l: List(a), f: fn(a) -> y.Yaml) -> y.Yaml {
  y.array(list.map(l, f))
}

fn import_to_yaml(definition: t.Definition(t.Import)) -> y.Yaml {
  todo
}

fn type_alias_to_yaml(definition: t.Definition(t.TypeAlias)) -> y.Yaml {
  todo
}

fn custom_type_to_yaml(definition: t.Definition(t.CustomType)) -> y.Yaml {
  todo
}

fn constant_to_yaml(def: t.Definition(t.ConstantDefinition)) -> y.Yaml {
  y.block([
    #("name", y.string(def.definition.name)),
    #("type", poly_to_yaml(def.definition.typ)),
    #("publicity", publicity_to_yaml(def.definition.publicity)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    // TODO: more
  ])
}

fn function_to_yaml(def: t.Definition(t.FunctionDefinition)) -> y.Yaml {
  y.block([
    #("name", y.string(def.definition.name)),
    #("type", poly_to_yaml(def.definition.typ)),
    #("publicity", publicity_to_yaml(def.definition.publicity)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    // TODO: more
  ])
}

fn poly_to_yaml(typ: t.Poly) -> y.Yaml {
  y.block([
    #("type_variables", ylist(typ.vars, type_var_id_to_yaml)),
    #("definition", type_to_yaml(typ.typ)),
  ])
}

fn type_to_yaml(typ: t.Type) -> y.Yaml {
  case typ {
    t.NamedType(module:, name:, parameters:) ->
      y.block([
        #("kind", y.string("named")),
        #("module", y.string(module)),
        #("name", y.string(name)),
        #("parameters", ylist(parameters, type_to_yaml)),
      ])
    t.TupleType(elements:) ->
      y.block([
        #("kind", y.string("tuple")),
        #("elements", ylist(elements, type_to_yaml)),
      ])
    t.FunctionType(parameters:, return:) ->
      y.block([
        #("kind", y.string("function")),
        #("parameters", ylist(parameters, type_to_yaml)),
        #("return", type_to_yaml(return)),
      ])
    t.VariableType(ref:) ->
      y.block([
        #("kind", y.string("variable")),
        #("id", type_var_id_to_yaml(ref)),
      ])
  }
}

fn type_var_id_to_yaml(type_var_id: t.TypeVarId) -> y.Yaml {
  y.int(type_var_id.id)
}

fn publicity_to_yaml(publicity: t.Publicity) -> y.Yaml {
  case publicity {
    t.Public -> "public"
    t.Private -> "private"
  }
  |> y.string
}

fn attribute_to_yaml(attribute: t.Attribute) -> y.Yaml {
  todo
}
