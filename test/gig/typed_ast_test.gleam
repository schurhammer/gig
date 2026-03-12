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

pub fn recursive_function_test() {
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
  |> birdie.snap(title: "recursive function test")
}

pub fn recursive_type_test() {
  let assert Ok(parsed) =
    glance.module(
      "
      pub type Tree(e) {
        Node(left: Tree(e), right: Tree(e))
        Leaf(data: e)
      }
      ",
    )

  echo "HI!"
  let assert Ok(checked) = t.infer_module(dict.new(), parsed, "tree") |> echo

  module_to_yaml(checked)
  |> y.encode
  |> birdie.snap(title: "recursive type test")
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

fn import_to_yaml(def: t.Definition(t.Import)) -> y.Yaml {
  let i = def.definition
  y.block(
    [
      Some(#("module", y.string(i.module))),
      option.map(i.alias, fn(alias) {
        #("alias", assignment_name_to_yaml(alias))
      }),
      Some(#("attributes", ylist(def.attributes, attribute_to_yaml))),
      Some(#(
        "unqualified_types",
        ylist(i.unqualified_types, unq_import_to_yaml),
      )),
      Some(#(
        "unqualified_values",
        ylist(i.unqualified_values, unq_import_to_yaml),
      )),
    ]
    |> option.values,
  )
}

fn type_alias_to_yaml(def: t.Definition(t.TypeAlias)) -> y.Yaml {
  let a = def.definition
  y.block([
    #("name", y.string(a.name)),
    #("type", poly_to_yaml(a.typ)),
    #("publicity", publicity_to_yaml(a.publicity)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    #("parameters", ylist(a.parameters, y.string)),
    #("aliased", annotation_to_yaml(a.aliased)),
  ])
}

fn custom_type_to_yaml(def: t.Definition(t.CustomType)) -> y.Yaml {
  let t = def.definition
  y.block([
    #("name", y.string(t.name)),
    #("type", poly_to_yaml(t.typ)),
    #("publicity", publicity_to_yaml(t.publicity)),
    #("opaque", y.bool(t.opaque_)),
    #("attributes", ylist(def.attributes, attribute_to_yaml)),
    #("parameters", ylist(t.parameters, y.string)),
    #("variants", ylist(t.variants, variant_to_yaml)),
  ])
}

fn variant_to_yaml(variant: t.Variant) -> y.Yaml {
  y.block([
    #("name", y.string(variant.name)),
    #("type", poly_to_yaml(variant.typ)),
    #(
      "fields",
      ylist(variant.fields, field_to_yaml(_, "annotation", annotation_to_yaml)),
    ),
  ])
}

fn constant_to_yaml(def: t.Definition(t.ConstantDefinition)) -> y.Yaml {
  let c = def.definition
  y.block(
    [
      Some(#("name", y.string(c.name))),
      Some(#("type", poly_to_yaml(c.typ))),
      option.map(c.annotation, fn(annotation) {
        #("annotation", annotation_to_yaml(annotation))
      }),
      Some(#("publicity", publicity_to_yaml(c.publicity))),
      Some(#("attributes", ylist(def.attributes, attribute_to_yaml))),
      Some(#("value", expr_to_yaml(c.value))),
    ]
    |> option.values,
  )
}

fn function_to_yaml(def: t.Definition(t.FunctionDefinition)) -> y.Yaml {
  let f = def.definition
  y.block(
    [
      Some(#("name", y.string(f.name))),
      Some(#("type", poly_to_yaml(f.typ))),
      Some(#("publicity", publicity_to_yaml(f.publicity))),
      Some(#("attributes", ylist(def.attributes, attribute_to_yaml))),
      Some(#("parameters", ylist(f.parameters, function_parameter_to_yaml))),
      option.map(f.return, fn(return) {
        #("return", annotation_to_yaml(return))
      }),
      Some(#("body", ylist(f.body, statement_to_yaml))),
      // TODO: location?
    ]
    |> option.values,
  )
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
              Ok(field) -> field_to_yaml(field, "value", expr_to_yaml)
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
      make(
        "fn_capture",
        typ,
        [
          option.map(label, fn(label) { #("label", y.string(label)) }),
          Some(#("function", expr_to_yaml(function))),
          Some(#(
            "arguments_before",
            ylist(arguments_before, field_to_yaml(_, "value", expr_to_yaml)),
          )),
          Some(#(
            "arguments_after",
            ylist(arguments_after, field_to_yaml(_, "value", expr_to_yaml)),
          )),
        ]
          |> option.values,
      )
    t.BitString(typ:, segments:) ->
      make("bit_string", typ, [
        #(
          "segments",
          ylist(segments, fn(segment) {
            y.block([
              #("expression", expr_to_yaml(segment.0)),
              #("options", ylist(segment.1, bsso_to_yaml(_, expr_to_yaml))),
            ])
          }),
        ),
      ])
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

fn field_to_yaml(
  field: t.Field(a),
  item_name: String,
  item_fun: fn(a) -> y.Yaml,
) {
  y.block(
    [
      option.map(field.label, fn(label) { #("label", y.string(label)) }),
      Some(#(item_name, item_fun(field.item))),
    ]
    |> option.values,
  )
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
      // TODO: why is it elems here and elements for list?
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
    t.PatternConcatenate(typ:, prefix:, prefix_name:, suffix_name:) ->
      make(
        "concatenate_pattern",
        typ,
        [
          Some(#("prefix", y.string(prefix))),
          option.map(prefix_name, fn(name) {
            #("prefix_name", assignment_name_to_yaml(name))
          }),
          Some(#("suffix_name", assignment_name_to_yaml(suffix_name))),
        ]
          |> option.values,
      )
    t.PatternBitString(typ:, segments:) ->
      make("bit_string_pattern", typ, [
        #(
          "segments",
          ylist(segments, fn(segment) {
            y.block([
              #("pattern", pattern_to_yaml(segment.0)),
              #("options", ylist(segment.1, bsso_to_yaml(_, pattern_to_yaml))),
            ])
          }),
        ),
      ])
    t.PatternConstructor(
      typ:,
      module:,
      constructor:,
      arguments: _,
      ordered_arguments:,
      with_module:,
      with_spread:,
    ) ->
      make("constructor_pattern", typ, [
        #("module", y.string(module)),
        #("constructor", y.string(constructor)),
        #(
          "arguments",
          ylist(ordered_arguments, field_to_yaml(_, "pattern", pattern_to_yaml)),
        ),
        #("with_module", y.bool(with_module)),
        #("with_spread", y.bool(with_spread)),
      ])
  }
}

fn function_parameter_to_yaml(function_parameter: t.FunctionParameter) -> y.Yaml {
  let t.FunctionParameter(typ:, label:, name:, annotation:) = function_parameter
  y.block(
    [
      Some(#("type", type_to_yaml(typ))),
      option.map(label, fn(label) { #("label", y.string(label)) }),
      Some(#("name", assignment_name_to_yaml(name))),
      option.map(annotation, fn(annotation) {
        #("annotation", annotation_to_yaml(annotation))
      }),
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
  y.string(
    "("
    <> string.join(list.map(typ.vars, fn(id) { int.to_string(id.id) }), ", ")
    <> ") "
    <> type_to_string(typ.typ),
  )
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
  y.string(anno_to_string(annotation))
}

fn anno_to_string(annotation: t.Annotation) -> String {
  let of_list = fn(l) { string.join(list.map(l, anno_to_string), ", ") }
  case annotation {
    t.NamedAnno(typ: _, module:, name:, parameters:) ->
      case module {
        Some(module) -> module <> "."
        None -> ""
      }
      <> name
      <> case parameters {
        [] -> ""
        _ -> "(" <> of_list(parameters) <> ")"
      }
    t.TupleAnno(typ: _, elements:) -> "#(" <> of_list(elements) <> ")"
    t.FunctionAnno(typ: _, parameters:, return:) ->
      "fn(" <> of_list(parameters) <> ") -> " <> anno_to_string(return)
    t.VariableAnno(typ: _, name:) -> name
    t.HoleAnno(typ: _, name:) -> "_" <> name
  }
}

fn attribute_to_yaml(attribute: t.Attribute) -> y.Yaml {
  todo
}

fn unq_import_to_yaml(unqualified_import: t.UnqualifiedImport) -> y.Yaml {
  todo
}

fn bsso_to_yaml(
  bit_string_segment_option: t.BitStringSegmentOption(a),
  fun: fn(a) -> y.Yaml,
) -> y.Yaml {
  todo
}
