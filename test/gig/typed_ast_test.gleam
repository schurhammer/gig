import birdie
import cymbal as y
import gig/gen_names
import gig/typed_ast as t
import glance
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

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
    // TODO: annotation?
    #("publicity", publicity_to_yaml(def.definition.publicity)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    #("value", expr_to_yaml(def.definition.value)),
  ])
}

fn function_to_yaml(def: t.Definition(t.FunctionDefinition)) -> y.Yaml {
  y.block([
    #("name", y.string(def.definition.name)),
    #("type", poly_to_yaml(def.definition.typ)),
    #("publicity", publicity_to_yaml(def.definition.publicity)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    #(
      "parameters",
      ylist(def.definition.parameters, function_parameter_to_yaml),
    ),
    // TODO: return
    #("body", ylist(def.definition.body, statement_to_yaml)),
    // TODO: location?
  ])
}

fn make(kind, typ, props) {
  y.block([#("kind", y.string(kind)), #("type", type_to_yaml(typ)), ..props])
}

fn statement_to_yaml(statement: t.Statement) -> y.Yaml {
  case statement {
    t.Use(typ:, patterns:, function:) -> todo
    t.Assignment(typ:, kind:, pattern:, annotation:, value:) -> todo
    t.Assert(typ:, expression:, message:) -> todo
    t.Expression(typ:, expression:) ->
      make("expression", typ, [#("expression", expr_to_yaml(expression))])
  }
}

fn expr_to_yaml(expr: t.Expression) -> y.Yaml {
  case expr {
    t.Int(typ:, value:) ->
      make("int_literal", typ, [#("value", y.string(value))])
    t.Float(typ:, value:) ->
      make("float_literal", typ, [#("value", y.string(value))])
    t.String(typ:, value:) ->
      make("string_literal", typ, [#("value", y.string(value))])
    t.LocalVariable(typ:, name:) ->
      make("local_variable", typ, [#("name", y.string(name))])
    t.Function(typ:, module:, name:, labels:) ->
      make("function", typ, [
        #("module", y.string(module)),
        #("name", y.string(name)),
        #(
          "labels",
          ylist(labels, fn(label) {
            case label {
              Some(label) -> y.string(label)
              None -> y.string("")
            }
          }),
        ),
      ])
    t.Constant(typ:, module:, name:) ->
      make("constant", typ, [
        #("module", y.string(module)),
        #("name", y.string(name)),
      ])
    t.NegateInt(typ:, value:) ->
      make("negate_int", typ, [#("value", expr_to_yaml(value))])
    t.NegateBool(typ:, value:) ->
      make("negate_bool", typ, [#("value", expr_to_yaml(value))])
    t.Block(typ:, statements:) ->
      make("block", typ, [
        #("statements", ylist(statements, statement_to_yaml)),
      ])
    t.Panic(typ:, value:) ->
      make("panic", typ, case value {
        Some(value) -> [#("value", expr_to_yaml(value))]
        None -> []
      })
    t.Todo(typ:, value:) ->
      make("todo", typ, case value {
        Some(value) -> [#("value", expr_to_yaml(value))]
        None -> []
      })
    t.Echo(typ:, value:) ->
      make("echo", typ, case value {
        Some(value) -> [#("value", expr_to_yaml(value))]
        None -> []
      })
    t.Tuple(typ:, elements:) ->
      make("tuple", typ, [
        #("elements", ylist(elements, expr_to_yaml)),
      ])
    t.List(typ:, elements:, rest:) ->
      make("list", typ, [
        #("elements", ylist(elements, expr_to_yaml)),
        ..case rest {
          Some(rest) -> [#("rest", expr_to_yaml(rest))]
          None -> []
        }
      ])
    t.Fn(typ:, parameters:, return:, body:) ->
      make("fn", typ, [
        #("parameters", ylist(parameters, function_parameter_to_yaml)),
      ])
    t.RecordUpdate(
      typ:,
      module: _,
      resolved_module:,
      constructor:,
      record:,
      fields:,
      ordered_fields:,
    ) ->
      make("record_update", typ, [
        #("module", y.string(resolved_module)),
        #("constructor", y.string(constructor)),
        #("record", expr_to_yaml(record)),
        #("fields", todo),
      ])
    t.FieldAccess(typ:, container:, module:, variant:, label:, index:) ->
      make("field_access", typ, [
        #("module", y.string(module)),
        // TODO: why call it constructor above and variant here?
        #("variant", y.string(variant)),
        #("label", y.string(label)),
        #("index", y.int(index)),
      ])
    t.Call(typ:, function:, ordered_arguments:) ->
      make("call", typ, [
        #("function", expr_to_yaml(function)),
        #("arguments", ylist(ordered_arguments, expr_to_yaml)),
      ])
    t.TupleIndex(typ:, tuple:, index:) ->
      make("tuple_index", typ, [
        #("tuple", expr_to_yaml(tuple)),
        #("index", y.int(index)),
      ])
    t.FnCapture(typ:, label:, function:, arguments_before:, arguments_after:) ->
      todo
    t.BitString(typ:, segments:) -> todo
    t.Case(typ:, subjects:, clauses:) ->
      make("case", typ, [
        #("subjects", ylist(subjects, expr_to_yaml)),
        #("clauses", ylist(clauses, clause_to_yaml)),
      ])
    t.BinaryOperator(typ:, name:, left:, right:) ->
      make("binary_operator", typ, [
        #("name", y.string(binop_to_string(name))),
        #("left", expr_to_yaml(left)),
        #("right", expr_to_yaml(right)),
      ])
  }
}

fn binop_to_string(op: glance.BinaryOperator) -> String {
  case op {
    glance.And -> todo
    glance.Or -> todo
    glance.Eq -> todo
    glance.NotEq -> todo
    glance.LtInt -> todo
    glance.LtEqInt -> todo
    glance.LtFloat -> todo
    glance.LtEqFloat -> todo
    glance.GtEqInt -> todo
    glance.GtInt -> todo
    glance.GtEqFloat -> todo
    glance.GtFloat -> todo
    glance.Pipe -> todo
    glance.AddInt -> "add_int"
    glance.AddFloat -> todo
    glance.SubInt -> todo
    glance.SubFloat -> todo
    glance.MultInt -> todo
    glance.MultFloat -> todo
    glance.DivInt -> todo
    glance.DivFloat -> todo
    glance.RemainderInt -> todo
    glance.Concatenate -> todo
  }
}

fn clause_to_yaml(clause: t.Clause) -> y.Yaml {
  y.block(
    [
      Some(#("patterns", ylist(clause.patterns, pattern_to_yaml))),
      option.map(clause.guard, fn(guard) { #("guard", expr_to_yaml(guard)) }),
      Some(#("body", expr_to_yaml(clause.body))),
    ]
    |> option.values,
  )
}

fn pattern_to_yaml(list: List(t.Pattern)) -> y.Yaml {
  todo
}

fn function_parameter_to_yaml(function_parameter: t.FunctionParameter) -> y.Yaml {
  let t.FunctionParameter(typ:, label:, name:, annotation:) = function_parameter
  y.block(
    [
      Some(#("type", type_to_yaml(typ))),
      option.map(label, fn(label) { #("label", y.string(label)) }),
      Some(#("name", assignment_name_to_yaml(name))),
      // TODO: annotation
    ]
    |> option.values,
  )
}

fn assignment_name_to_yaml(name: t.AssignmentName) -> y.Yaml {
  case name {
    t.Named(value:) -> value
    t.Discarded(value:) -> "_" <> value
  }
  |> y.string
}

fn poly_to_yaml(typ: t.Poly) -> y.Yaml {
  y.block([
    #("type_variables", ylist(typ.vars, type_var_id_to_yaml)),
    #("definition", type_to_yaml(typ.typ)),
  ])
}

fn type_to_string(typ: t.Type) -> String {
  let of_list = fn(l) { string.join(list.map(l, type_to_string), ", ") }
  case typ {
    t.NamedType(module:, name:, parameters:) -> {
      module
      <> "."
      <> name
      <> case parameters {
        [] -> ""
        _ -> "(" <> of_list(parameters) <> ")"
      }
    }
    t.TupleType(elements:) -> {
      "#(" <> of_list(elements) <> ")"
    }
    t.FunctionType(parameters:, return:) -> {
      "fn(" <> of_list(parameters) <> ") -> " <> type_to_string(return)
    }
    t.VariableType(ref:) -> int.to_string(ref.id)
  }
}

fn type_to_yaml(typ: t.Type) -> y.Yaml {
  y.string(type_to_string(typ))
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

fn annotation_to_yaml(annotation: option.Option(t.Annotation)) -> y.Yaml {
  todo
}

fn attribute_to_yaml(attribute: t.Attribute) -> y.Yaml {
  todo
}
