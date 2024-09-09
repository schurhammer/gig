import env
import gig/gen_names.{get_id}
import gig/typed_ast as t
import glance as g
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import pprint

pub const builtin = t.builtin

pub type Type {
  NamedType(id: String, parameters: List(Type))
  TupleType(elements: List(Type))
  FunctionType(parameters: List(Type), return: Type)
  Unbound(id: Int)
}

pub type Poly {
  Poly(vars: List(Int), typ: Type)
}

pub type Parameter {
  Parameter(typ: Type, name: String)
}

pub type LiteralKind {
  Int(value: String)
  Float(value: String)
  String(value: String)
}

pub type Exp {
  Literal(typ: Type, value: LiteralKind)
  Local(typ: Type, name: String)
  Global(typ: Type, id: String)
  Fn(typ: Type, parameters: List(Parameter), body: Exp)
  Call(typ: Type, function: Exp, arguments: List(Exp))
  // TODO we could give every let binding a unique integer id
  // to solve any shadowing issues (maybe in typed_ast?)
  Let(typ: Type, name: String, value: Exp, body: Exp)
  If(typ: Type, condition: Exp, then: Exp, els: Exp)
  Panic(typ: Type, reason: Exp)
}

pub type CustomType {
  CustomType(
    typ: Poly,
    id: String,
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type Variant {
  // TODO bring back field names?
  Variant(typ: Poly, id: String, fields: List(Type))
}

pub type Function {
  Function(
    typ: Poly,
    id: String,
    parameters: List(Parameter),
    body: Exp,
    external: Bool,
    monomorphise: Bool,
  )
}

pub type Context {
  Context(
    types: env.Env(String, CustomType),
    functions: env.Env(String, Function),
  )
}

// monadic form
// pub type Value {
//   Literal(value: String)
//   Variable(name: String)
//   Global(name: String)
//   Fn(parameters: List(String), body: Computation)
// }

// pub type Computation {
//   Value(value: Value)
//   Call(function: Value, arguments: List(Value))
//   Let(name: String, value: Computation, body: Computation)
//   If(condition: Value, then: Computation, els: Computation)
// }

pub fn lower_context(c: t.Context) {
  let acc = Context(env.new(), env.new())
  env.fold(c.modules, acc, fn(acc, name, module) {
    lower_module(c, acc, module)
  })
}

fn lower_module(c: t.Context, acc: Context, module: t.Module) {
  let c = t.Context(..c, current_module: module.name)

  let acc =
    list.fold(module.custom_types, acc, fn(acc, custom) {
      case custom.definition.variants {
        // no variants is considered external
        [] -> acc
        _ -> {
          let custom = lower_custom_type(c, custom.definition)
          Context(..acc, types: env.put(acc.types, custom.id, custom))
        }
      }
    })

  let acc =
    list.fold(module.functions, acc, fn(acc, fun) {
      let fun = lower_function(c, fun)
      Context(..acc, functions: env.put(acc.functions, fun.id, fun))
    })
  acc
}

fn lower_custom_type(c: t.Context, custom: t.CustomType) {
  let typ = map_poly(c, custom.typ)
  let module = c.current_module
  let name = custom.name
  let id = get_id(module, name)
  let parameters = custom.parameters
  let variants =
    list.map(custom.variants, fn(variant) {
      let typ = map_poly(c, variant.typ)
      let fields =
        list.map(variant.fields, fn(field) { map_type(c, field.item.typ) })
      let id = get_id(module, variant.name)
      Variant(typ, id, fields)
    })
  CustomType(typ, id, parameters, variants)
}

fn lower_function(c: t.Context, def: t.Definition(t.Function)) {
  let function = def.definition
  let typ = map_poly(c, function.typ)
  let module = c.current_module
  let name = function.name
  let id = get_id(module, name)
  let parameters = list.map(function.parameters, lower_parameter(c, _))
  let body = lower_body(c, function.body)

  let external = list.any(def.attributes, fn(x) { x.name == "external" })
  let monomorphise =
    list.any(def.attributes, fn(x) { x.name == "monomorphise" })
  let monomorphise = monomorphise || !external

  Function(typ:, id:, parameters:, body:, external:, monomorphise:)
}

fn lower_parameter(c: t.Context, parameter: t.FunctionParameter) {
  let typ = map_type(c, parameter.typ)
  let name = case parameter.name {
    t.Named(name) -> name
    t.Discarded(name) -> "_" <> name
  }
  Parameter(typ, name)
}

fn lower_body(c: t.Context, body: List(t.Statement)) {
  case body {
    [] -> lower_expression(c, t.Todo(t.nil_type, None))
    [statement] ->
      case statement {
        t.Expression(_, exp) -> lower_expression(c, exp)
        t.Assignment(_, _, _, _, value) -> lower_expression(c, value)
        t.Use(..) -> todo
      }
    [statement, ..body] ->
      case statement {
        t.Expression(_, value) -> {
          let value = lower_expression(c, value)
          let body = lower_body(c, body)
          Let(value.typ, "_", value, body)
        }
        t.Assignment(_, _, pattern, _, value) -> {
          let subject = t.LocalVariable(value.typ, "subject")
          let value = lower_expression(c, value)
          let body = lower_body(c, body)
          let bindings = lower_pattern_bindings(c, pattern, subject)
          let body =
            list.fold(bindings, body, fn(body, binding) {
              let #(name, subject) = binding
              let value = lower_expression(c, subject)
              Let(body.typ, name, value, body)
            })
          Let(value.typ, "subject", value, body)
        }
        t.Use(..) -> todo
      }
  }
}

fn lower_pattern_bindings(
  c: t.Context,
  pattern: t.Pattern,
  subject: t.Expression,
) -> List(#(String, t.Expression)) {
  case pattern {
    t.PatternInt(..) -> []
    t.PatternFloat(..) -> []
    t.PatternString(..) -> []
    t.PatternDiscard(..) -> []
    t.PatternVariable(typ, name) -> [#(name, subject)]
    t.PatternTuple(typ, elems) -> todo
    t.PatternList(typ, elements, tail) -> todo
    t.PatternAssignment(typ, pattern, name) -> {
      let pattern = lower_pattern_bindings(c, pattern, subject)
      [#(name, subject), ..pattern]
    }
    t.PatternConcatenate(typ, left, right) -> todo
    t.PatternBitString(typ, segs) -> todo
    t.PatternConstructor(
      typ,
      module,
      constructor,
      arguments,
      ordered_arguments,
      _,
      _,
    ) -> {
      let elems: List(t.Pattern) = ordered_arguments
      list.index_map(elems, fn(elem, i) {
        let subject =
          t.FieldAccess(elem.typ, subject, module, constructor, "", i)
        lower_pattern_bindings(c, elem, subject)
      })
      |> list.flatten
    }
  }
}

const bool_type = NamedType("Bool", [])

const true_value = Global(bool_type, "True")

fn and_exp(first: Exp, second: Exp) {
  case first, second {
    Global(_, "True"), _ -> second
    _, Global(_, "True") -> first
    _, _ -> {
      let fun_typ = FunctionType([bool_type, bool_type], bool_type)
      let fun = Global(fun_typ, "and_bool")
      let args = [first, second]
      Call(bool_type, fun, args)
    }
  }
}

fn tuple_type_name(typ: Type) {
  case typ {
    TupleType(e) -> "Tuple" <> int.to_string(list.length(e))
    _ -> panic as "expected tuple type"
  }
}

fn lower_pattern_match(
  c: t.Context,
  pattern: t.Pattern,
  subject: t.Expression,
) -> Exp {
  case pattern {
    t.PatternInt(typ, value) -> {
      let value = t.Int(typ, value)
      let match = t.BinaryOperator(t.bool_type, g.Eq, subject, value)
      lower_expression(c, match)
    }
    t.PatternFloat(typ, value) -> {
      let value = t.Float(typ, value)
      let match = t.BinaryOperator(t.bool_type, g.Eq, subject, value)
      lower_expression(c, match)
    }
    t.PatternString(typ, value) -> {
      let value = t.String(typ, value)
      let match = t.BinaryOperator(t.bool_type, g.Eq, subject, value)
      lower_expression(c, match)
    }
    t.PatternDiscard(typ, name) -> true_value
    t.PatternVariable(typ, name) -> true_value
    t.PatternTuple(typ, elems) -> {
      // rewrite to constructor pattern
      // TODO check the order of boolean expression is correct
      // maybe we need to reverse, swap params, fold_right etc
      list.index_fold(elems, true_value, fn(match, elem, i) {
        let subject = t.TupleIndex(elem.typ, subject, i)
        let elem_match = lower_pattern_match(c, elem, subject)
        and_exp(elem_match, match)
      })
    }
    t.PatternList(typ, elements, tail) -> {
      // rewrite to constructor pattern
      let tail = case tail {
        Some(tail) -> tail
        None ->
          t.PatternConstructor(typ, t.builtin, "Empty", [], [], True, False)
      }
      let elements = list.reverse(elements)
      let list =
        list.fold(elements, tail, fn(rest, x) {
          let a = [t.Field(None, x), t.Field(None, rest)]
          let o = [x, rest]
          t.PatternConstructor(typ, t.builtin, "Cons", a, o, True, False)
        })
      lower_pattern_match(c, list, subject)
    }
    t.PatternAssignment(typ, pattern, name) -> true_value
    t.PatternConcatenate(typ, left, right) -> todo as "PatternConcatenate"
    t.PatternBitString(typ, segs) -> todo as "PatternBitString"
    t.PatternConstructor(
      typ,
      module,
      constructor,
      arguments,
      ordered_arguments,
      _,
      _,
    ) -> {
      let elems: List(t.Pattern) = ordered_arguments
      list.index_fold(elems, true_value, fn(match, elem, i) {
        let subject =
          t.FieldAccess(elem.typ, subject, module, constructor, "", i)
        let elem_match = lower_pattern_match(c, elem, subject)
        and_exp(elem_match, match)
      })
    }
  }
}

fn lower_expression(c: t.Context, exp: t.Expression) -> Exp {
  case exp {
    t.Int(typ, value) -> Literal(map_type(c, typ), Int(value))
    t.Float(typ, value) -> Literal(map_type(c, typ), Float(value))
    t.String(typ, value) -> Literal(map_type(c, typ), String(value))
    t.LocalVariable(typ, name) -> Local(map_type(c, typ), name)
    t.GlobalVariable(typ, module, name) -> {
      let assert Ok(#(_, _, _, kind)) = t.resolve_global_name(c, module, name)

      // rewrite function name if its a constructor function
      // TODO maybe change type naming scheme so we can keep constructor names
      let name = case kind {
        t.ConstructorFunction -> gen_names.get_constructor_name(name)
        _ -> name
      }

      let typ = map_type(c, typ)
      Global(typ, get_id(module, name))
    }
    t.NegateInt(typ, value) -> {
      let typ = map_type(c, typ)
      let value = lower_expression(c, value)
      let fun = Global(FunctionType([typ], typ), "negate_int")
      Call(typ, fun, [value])
    }
    t.NegateBool(typ, value) -> {
      let typ = map_type(c, typ)
      let value = lower_expression(c, value)
      let fun = Global(FunctionType([typ], typ), "negate_bool")
      Call(typ, fun, [value])
    }
    t.Block(_typ, statements) -> lower_body(c, statements)
    t.Panic(typ, value) -> {
      let typ = map_type(c, typ)
      let value = case value {
        Some(value) -> lower_expression(c, value)
        None -> Literal(map_type(c, t.string_type), String("panic"))
      }
      Panic(typ, value)
    }
    t.Todo(typ, value) -> {
      let typ = map_type(c, typ)
      let value = case value {
        Some(value) -> lower_expression(c, value)
        None -> Literal(map_type(c, t.string_type), String("todo"))
      }
      Panic(typ, value)
    }
    t.Tuple(typ, elements) -> {
      let typ = map_type(c, typ)
      let elements = list.map(elements, lower_expression(c, _))
      let len = int.to_string(list.length(elements))
      let fun = Global(FunctionType([typ], typ), "Tuple" <> len)
      Call(typ, fun, elements)
    }
    t.List(typ, elements, rest) -> {
      let list_typ = map_type(c, typ)
      let rest = case rest {
        Some(rest) -> lower_expression(c, rest)
        None -> Global(list_typ, "Empty")
      }
      let elements = list.reverse(elements)
      list.fold(elements, rest, fn(rest, element) {
        let element = lower_expression(c, element)
        let args = [element, rest]
        let cons_typ = FunctionType([element.typ, list_typ], list_typ)
        let cons = Global(cons_typ, "Cons")
        Call(list_typ, cons, args)
      })
    }
    t.Fn(typ, parameters, return, body) -> {
      let typ = map_type(c, typ)
      let parameters = list.map(parameters, lower_parameter(c, _))
      let body = lower_body(c, body)
      Fn(typ, parameters, body)
    }
    t.RecordUpdate(typ, module, constructor, record, fields) -> todo
    t.FieldAccess(typ, container, module, variant, label, index) -> {
      let typ = map_type(c, typ)
      let container = lower_expression(c, container)
      let getter_name = gen_names.get_getter_name(variant, index)
      let getter_typ = FunctionType([container.typ], typ)
      let getter = Global(getter_typ, get_id(module, getter_name))
      Call(typ, getter, [container])
    }
    t.Call(typ, function, _, ordered_arguments) -> {
      let typ = map_type(c, typ)
      let function = lower_expression(c, function)
      let arguments = list.map(ordered_arguments, lower_expression(c, _))
      Call(typ, function, arguments)
    }
    t.TupleIndex(typ, tuple, index) -> {
      let typ = map_type(c, typ)
      let tuple = lower_expression(c, tuple)
      let constructor = tuple_type_name(tuple.typ)
      let getter_name = gen_names.get_getter_name(constructor, index)
      let getter_typ = FunctionType([tuple.typ], typ)
      let getter = Global(getter_typ, getter_name)
      Call(typ, getter, [tuple])
    }
    t.FnCapture(typ, label, function, arguments_before, arguments_after) -> todo
    t.BitString(typ, segs) -> todo
    t.Case(typ, subjects, clauses) -> {
      let typ = map_type(c, typ)
      let else_body =
        Panic(
          typ,
          Literal(map_type(c, t.string_type), String("No matching clause")),
        )

      // Create bindings for each subject
      let subject_vars =
        list.index_map(subjects, fn(subject, index) {
          // TODO special case for when subject is already a local
          let name = "subject_" <> int.to_string(index)
          let subject_exp = lower_expression(c, subject)
          #(name, subject, subject_exp)
        })

      // Create the main body of the case expression
      let case_body =
        list.fold_right(clauses, else_body, fn(acc, clause) {
          // Process each pattern list in the clause
          list.fold_right(clause.patterns, acc, fn(clause_acc, patterns) {
            let assert Ok(sub_pats) = list.strict_zip(subject_vars, patterns)

            // Find bindings for the matched patterns
            let bindings =
              list.flat_map(sub_pats, fn(pair) {
                let #(#(name, exp, _), pattern) = pair
                let sub = t.LocalVariable(exp.typ, name)
                lower_pattern_bindings(c, pattern, sub)
              })

            // Create pattern matching conditions
            let conditions =
              list.map(sub_pats, fn(pair) {
                let #(#(name, exp, _), pattern) = pair
                let sub = t.LocalVariable(exp.typ, name)
                lower_pattern_match(c, pattern, sub)
              })

            // Add guard to conditions if present
            let conditions = case clause.guard {
              Some(guard) -> {
                let guard = lower_expression(c, guard)
                // Inline bindings in guard
                let guard =
                  list.fold(bindings, guard, fn(guard, binding) {
                    let #(name, value) = binding
                    let value = lower_expression(c, value)
                    replace_var(name, value, guard)
                  })
                list.append(conditions, [guard])
              }
              None -> conditions
            }

            // Apply the bindings to the body
            let body = lower_expression(c, clause.body)
            let body =
              list.fold_right(bindings, body, fn(body_acc, binding) {
                let #(name, value) = binding
                let value = lower_expression(c, value)
                Let(body_acc.typ, name, value, body_acc)
              })

            let condition = list.fold_right(conditions, true_value, and_exp)

            If(typ, condition, body, clause_acc)
          })
        })

      // Wrap the case body with let expressions for subject bindings
      list.fold_right(subject_vars, case_body, fn(acc, subject_var) {
        let #(name, _, value) = subject_var
        Let(typ, name, value, acc)
      })
    }
    t.BinaryOperator(typ, name, left, right) -> {
      let typ = map_type(c, typ)
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      let function_name = case name {
        g.And -> "and_bool"
        g.Or -> "or_bool"
        g.Eq -> "eq"
        g.NotEq -> "not_eq"
        g.LtInt -> "lt_int"
        g.LtEqInt -> "lte_int"
        g.LtFloat -> "lt_float"
        g.LtEqFloat -> "lte_float"
        g.GtEqInt -> "gte_int"
        g.GtInt -> "gt_int"
        g.GtEqFloat -> "gte_float"
        g.GtFloat -> "gt_float"
        g.Pipe -> todo as "pipe"
        g.AddInt -> "add_int"
        g.AddFloat -> "add_float"
        g.SubInt -> "sub_int"
        g.SubFloat -> "sub_float"
        g.MultInt -> "mul_int"
        g.MultFloat -> "mul_float"
        g.DivInt -> "div_int"
        g.DivFloat -> "div_float"
        g.RemainderInt -> "rem_int"
        g.Concatenate -> "append_string"
      }
      let function_type = FunctionType([left.typ, right.typ], typ)
      let function = Global(function_type, function_name)
      Call(typ, function, [left, right])
    }
  }
}

fn replace_var(replace: String, with: Exp, in: Exp) -> Exp {
  case in {
    Literal(_, _) -> in
    Global(_, _) -> in
    Local(_typ, var) ->
      case var == replace {
        True -> with
        False -> in
      }
    Call(typ, fun, args) -> {
      let fun = replace_var(replace, with, fun)
      let args = list.map(args, replace_var(replace, with, _))
      Call(typ, fun, args)
    }
    Fn(typ, vars, exp) ->
      case list.find(vars, fn(v) { v.name == replace }) {
        Ok(_) -> in
        Error(_) -> Fn(typ, vars, replace_var(replace, with, exp))
      }
    Let(typ, var, val, exp) ->
      case var == replace {
        True -> Let(typ, var, replace_var(replace, with, val), exp)
        False ->
          Let(
            typ,
            var,
            replace_var(replace, with, val),
            replace_var(replace, with, exp),
          )
      }
    If(typ, cond, then_exp, else_exp) ->
      If(
        typ,
        replace_var(replace, with, cond),
        replace_var(replace, with, then_exp),
        replace_var(replace, with, else_exp),
      )
    Panic(typ, e) -> Panic(typ, replace_var(replace, with, e))
  }
}

fn map_poly(c: t.Context, typ: t.Poly) {
  Poly(typ.vars, map_type(c, typ.typ))
}

fn map_type(c: t.Context, typ: t.Type) {
  case typ {
    t.NamedType(name:, module:, parameters:) -> {
      let parameters = list.map(parameters, map_type(c, _))
      NamedType(get_id(module, name), parameters)
    }
    t.FunctionType(parameters, return) -> {
      let parameters = list.map(parameters, map_type(c, _))
      let return = map_type(c, return)
      FunctionType(parameters, return)
    }
    t.TupleType(elements) -> {
      let elements = list.map(elements, map_type(c, _))
      TupleType(elements)
    }
    t.VariableType(ref) -> {
      let assert Ok(x) = env.get(c.type_vars, ref)
      case x {
        t.Bound(x) -> map_type(c, x)
        t.Unbound(x) -> Unbound(x)
      }
    }
  }
}
