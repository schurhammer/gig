import birdie
import cymbal as y
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

pub fn recursion_test() {
  let assert Ok(parsed) =
    glance.module(
      "
      pub fn fold(list, acc, fun) {
        case list {
          [] -> acc
          [head, ..tail] -> fold(tail, fun(acc, head), fun)
        }
      }
      ",
    )

  let assert Ok(checked) = t.infer_module(dict.new(), parsed, "fold")

  module_to_yaml(checked)
  |> y.encode
  |> birdie.snap(title: "recursion test")
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
    t.Use(typ:, patterns:, function:) ->
      make("use", typ, [
        #("patterns", ylist(patterns, pattern_to_yaml)),
        #("function", expr_to_yaml(function)),
      ])
    t.Assignment(typ:, kind:, pattern:, annotation:, value:) ->
      make(
        "assignment",
        typ,
        [
          Some(#(
            "assignment_kind",
            case kind {
              t.Let -> "let"
              t.LetAssert -> "let_assert"
            }
              |> y.string,
          )),
          Some(#("pattern", pattern_to_yaml(pattern))),
          option.map(annotation, fn(annotation) {
            #("annotation", annotation_to_yaml(annotation))
          }),
          Some(#("value", expr_to_yaml(value))),
        ]
          |> option.values,
      )
    t.Assert(typ:, expression:, message:) ->
      make(
        "assert",
        typ,
        [
          Some(#("expression", expr_to_yaml(expression))),
          option.map(message, fn(message) {
            #("message", expr_to_yaml(message))
          }),
        ]
          |> option.values,
      )
    t.Expression(typ: _, expression:) -> expr_to_yaml(expression)
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
    t.Function(typ:, module:, name:, labels: _) ->
      make("function", typ, [
        #("module", y.string(module)),
        #("name", y.string(name)),
        // TODO: do we need labels?
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
      make(
        "fn",
        typ,
        [
          Some(#("parameters", ylist(parameters, function_parameter_to_yaml))),
          option.map(return, fn(return) {
            #("return", annotation_to_yaml(return))
          }),
          Some(#("body", ylist(body, statement_to_yaml))),
        ]
          |> option.values,
      )
    t.RecordUpdate(
      typ:,
      module: _,
      resolved_module:,
      constructor:,
      record:,
      fields: _,
      ordered_fields:,
    ) ->
      make("record_update", typ, [
        #("module", y.string(resolved_module)),
        #("constructor", y.string(constructor)),
        #("record", expr_to_yaml(record)),
        #(
          "fields",
          ylist(ordered_fields, fn(field) {
            case field {
              Ok(field) -> field_to_yaml(field)
              Error(typ) -> y.block([#("type", type_to_yaml(typ))])
            }
          }),
        ),
      ])
    t.FieldAccess(typ:, container:, module:, variant:, label:, index:) ->
      make("field_access", typ, [
        #("container", expr_to_yaml(container)),
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
    glance.And -> "&&"
    glance.Or -> "||"
    glance.Eq -> "=="
    glance.NotEq -> "!="
    glance.LtInt -> "<"
    glance.LtEqInt -> "<="
    glance.LtFloat -> "<."
    glance.LtEqFloat -> "<=."
    glance.GtEqInt -> ">="
    glance.GtInt -> ">"
    glance.GtEqFloat -> ">=."
    glance.GtFloat -> ">"
    glance.Pipe -> "|>"
    glance.AddInt -> "+"
    glance.AddFloat -> "+."
    glance.SubInt -> "-"
    glance.SubFloat -> "-."
    glance.MultInt -> "*"
    glance.MultFloat -> "*."
    glance.DivInt -> "/"
    glance.DivFloat -> "/."
    glance.RemainderInt -> "%"
    glance.Concatenate -> "<>"
  }
}

fn clause_to_yaml(clause: t.Clause) -> y.Yaml {
  y.block(
    [
      Some(#("patterns", ylist(clause.patterns, ylist(_, pattern_to_yaml)))),
      option.map(clause.guard, fn(guard) { #("guard", expr_to_yaml(guard)) }),
      Some(#("body", expr_to_yaml(clause.body))),
    ]
    |> option.values,
  )
}

fn pattern_to_yaml(list: t.Pattern) -> y.Yaml {
  case list {
    t.PatternInt(typ:, value:) ->
      make("int_pattern", typ, [#("value", y.string(value))])
    t.PatternFloat(typ:, value:) ->
      make("float_pattern", typ, [#("value", y.string(value))])
    t.PatternString(typ:, value:) ->
      make("string_pattern", typ, [#("value", y.string(value))])
    t.PatternDiscard(typ:, name:) ->
      make("discard_pattern", typ, [#("name", y.string(name))])
    t.PatternVariable(typ:, name:) ->
      make("variable_pattern", typ, [#("name", y.string(name))])
    t.PatternTuple(typ:, elems:) ->
      make("tuple_pattern", typ, [#("elements", ylist(elems, pattern_to_yaml))])
    t.PatternList(typ:, elements:, tail:) ->
      make(
        "list_pattern",
        typ,
        [
          Some(#("elements", ylist(elements, pattern_to_yaml))),
          option.map(tail, fn(tail) { #("tail", pattern_to_yaml(tail)) }),
        ]
          |> option.values,
      )
    t.PatternAssignment(typ:, pattern:, name:) ->
      make("assignment_pattern", typ, [
        #("name", y.string(name)),
        #("pattern", pattern_to_yaml(pattern)),
      ])
    t.PatternConcatenate(typ:, prefix:, prefix_name:, suffix_name:) -> todo
    t.PatternBitString(typ:, segments:) -> todo
    t.PatternConstructor(
      typ:,
      module:,
      constructor:,
      arguments:,
      ordered_arguments:,
      with_module:,
      with_spread:,
    ) -> todo
  }
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

fn field_to_yaml(field: t.Field(t.Expression)) -> y.Yaml {
  let t.Field(label:, item:) = field
  y.block(
    [
      option.map(label, fn(label) { #("label", y.string(label)) }),
      Some(#("item", expr_to_yaml(item))),
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
    #("parameters", ylist(typ.vars, type_var_id_to_yaml)),
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

fn annotation_to_yaml(annotation: t.Annotation) -> y.Yaml {
  todo
}

fn attribute_to_yaml(attribute: t.Attribute) -> y.Yaml {
  todo
}
