import gig/gen_names.{get_id}
import gig/typed_ast as t
import gleam/dict.{type Dict}
import gleam/io
import pprint

import glance as g

import gleam/int
import gleam/list
import gleam/option.{None, Some}

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
  NilVal
  Bool(value: String)
  Int(value: String)
  Float(value: String)
  String(value: String)
  BitArray(size: String)
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
  CustomType(typ: Poly, id: String, variants: List(Variant))
}

pub type Variant {
  // TODO bring back field names?
  Variant(typ: Poly, id: String, fields: List(Type))
}

pub type Function {
  Function(typ: Poly, id: String, parameters: List(Parameter), body: Exp)
}

pub type External {
  External(typ: Poly, id: String, mono: Bool)
}

pub type Context {
  Context(
    types: Dict(String, CustomType),
    functions: Dict(String, Function),
    externals: Dict(String, External),
  )
}

// TODO monadic form
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
  // these need registered because they are converted to lieterals
  let externals =
    dict.new()
    |> dict.insert("Nil", External(Poly([], nil_type), "Nil", False))
    |> dict.insert("True", External(Poly([], bool_type), "True", False))
    |> dict.insert("False", External(Poly([], bool_type), "False", False))

  let acc = Context(types: dict.new(), functions: dict.new(), externals:)
  dict.fold(c.modules, acc, fn(acc, name, module) {
    lower_module(c, acc, module)
  })
}

fn lower_module(c: t.Context, acc: Context, module: t.Module) {
  let c = t.Context(..c, current_module: module.name)

  // create type related builtin functions
  let acc =
    list.fold(module.custom_types, acc, fn(acc, def) {
      let custom = def.definition
      list.fold(custom.variants, acc, fn(acc, variant) {
        register_variant_functions(c, acc, module.name, custom.typ, variant)
      })
    })

  // remove some types that are replaced with builtins
  let custom_types = case module.name == t.builtin {
    True -> {
      module.custom_types
      |> list.filter(fn(c) {
        case c.definition.name {
          "Nil" -> False
          "Bool" -> False
          _ -> True
        }
      })
    }
    False -> module.custom_types
  }

  let acc =
    list.fold(custom_types, acc, fn(acc, custom) {
      case custom.definition.variants {
        // no variants is considered external
        [] -> acc
        _ -> {
          let custom = lower_custom_type(c, custom.definition)
          Context(..acc, types: dict.insert(acc.types, custom.id, custom))
        }
      }
    })

  let acc =
    list.fold(module.functions, acc, fn(acc, fun) {
      let attrs = fun.attributes
      let external = list.find(attrs, fn(x) { x.name == "external" })

      case external {
        Ok(external) -> {
          let assert t.Attribute(
            _,
            [t.LocalVariable(_, "c"), _, t.String(_, external_id)],
          ) = external
          let typ = map_poly(c, fun.definition.typ)
          let module = c.current_module
          let name = fun.definition.name
          let internal_id = get_id(module, name)
          let mono = list.any(attrs, fn(x) { x.name == "monomorphise" })
          let fun = External(typ, external_id, mono)
          Context(
            ..acc,
            externals: dict.insert(acc.externals, internal_id, fun),
          )
        }
        Error(_) -> {
          let fun = lower_function(c, fun)
          Context(..acc, functions: dict.insert(acc.functions, fun.id, fun))
        }
      }
    })
  acc
}

fn lower_custom_type(c: t.Context, custom: t.CustomType) {
  let typ = map_poly(c, custom.typ)
  let module = c.current_module
  let name = custom.name
  let id = get_id(module, name)
  let variants =
    list.map(custom.variants, fn(variant) {
      let typ = map_poly(c, variant.typ)
      let fields =
        list.map(variant.fields, fn(field) { map_type(c, field.item.typ) })
      let id = get_id(module, variant.name)
      Variant(typ, id, fields)
    })
  CustomType(typ, id, variants)
}

fn lower_function(c: t.Context, def: t.Definition(t.Function)) {
  let function = def.definition
  let typ = map_poly(c, function.typ)
  let module = c.current_module
  let name = function.name
  let id = get_id(module, name)
  let parameters = list.map(function.parameters, lower_parameter(c, _))
  let body = lower_body(c, function.body)
  let #(_, _, body) = unshadow([], 1, body)

  Function(typ:, id:, parameters:, body:)
}

fn lower_parameter(c: t.Context, parameter: t.FunctionParameter) {
  let typ = map_type(c, parameter.typ)
  let name = case parameter.name {
    t.Named(name) -> name
    t.Discarded(name) -> "_" <> name
  }
  Parameter(typ, name)
}

pub fn register_variant_functions(
  c: t.Context,
  acc: Context,
  module_name: String,
  custom_typ: t.Poly,
  variant: t.Variant,
) {
  // constructor function
  let variant_id = get_id(module_name, variant.name)
  let typ = map_poly(c, variant.typ)
  let fun = External(typ: typ, id: "new_" <> variant_id, mono: True)
  let acc =
    Context(..acc, externals: dict.insert(acc.externals, variant_id, fun))

  // variant check function
  let fun_id = gen_names.get_variant_check_name(variant_id)
  let typ = map_poly(c, custom_typ)
  let typ = Poly(typ.vars, FunctionType([typ.typ], bool_type))
  let fun = External(typ: typ, id: fun_id, mono: True)
  let acc = Context(..acc, externals: dict.insert(acc.externals, fun.id, fun))

  // getter functions
  list.index_fold(variant.fields, acc, fn(acc, f, i) {
    let fun_id = gen_names.get_getter_name(variant_id, i)
    let typ = map_poly(c, custom_typ)
    let field_typ = map_type(c, f.item.typ)
    let typ = Poly(typ.vars, FunctionType([typ.typ], field_typ))
    let fun = External(typ: typ, id: fun_id, mono: True)
    Context(..acc, externals: dict.insert(acc.externals, fun.id, fun))
  })
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
          Let(body.typ, "_", value, body)
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
          Let(body.typ, "subject", value, body)
        }
        t.Use(..) -> todo
      }
  }
}

fn index_bit_array(options, subject, offset) {
  let #(size, bits) =
    list.fold(options, #(-1, False), fn(acc, option) {
      let #(size, bits) = acc
      case option {
        t.BytesOption | t.BitsOption -> #(size, True)
        t.SizeOption(size) -> #(size, bits)
        t.SizeValueOption(e) -> {
          case e {
            t.PatternInt(_, size) -> {
              let assert Ok(size) = int.parse(size)
              #(size, bits)
            }
            _ -> todo
          }
        }
        _ -> todo
      }
    })

  case bits {
    True -> {
      // slice_bit_array knows how to handle -1
      // it is interpreted as "to the end"
      // so we only set the size afterwards
      let inner_subject =
        t.Call(
          t.bit_array_type,
          t.GlobalVariable(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.bit_array_type,
            ),
            t.builtin,
            "slice_bit_array",
          ),
          [t.Field(None, subject)],
          [
            subject,
            t.Int(t.int_type, int.to_string(offset)),
            t.Int(t.int_type, int.to_string(size)),
          ],
        )
      #(size, inner_subject)
    }
    False -> {
      let size = case size {
        -1 -> 8
        _ -> size
      }
      let inner_subject =
        t.Call(
          t.int_type,
          t.GlobalVariable(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.int_type,
            ),
            t.builtin,
            "index_bit_array_int",
          ),
          [t.Field(None, subject)],
          [
            subject,
            t.Int(t.int_type, int.to_string(offset)),
            t.Int(t.int_type, int.to_string(size)),
          ],
        )
      #(size, inner_subject)
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
    t.PatternTuple(typ, elems) -> {
      list.index_map(elems, fn(elem, i) {
        let subject = t.TupleIndex(elem.typ, subject, i)
        lower_pattern_bindings(c, elem, subject)
      })
      |> list.flatten
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
      lower_pattern_bindings(c, list, subject)
    }
    t.PatternAssignment(typ, pattern, name) -> {
      let pattern = lower_pattern_bindings(c, pattern, subject)
      [#(name, subject), ..pattern]
    }
    t.PatternConcatenate(typ, left, right) -> todo
    t.PatternBitString(typ, segs) -> {
      let #(total_size, segs) =
        list.fold(segs, #(0, []), fn(acc, seg) {
          let #(offset, bindings) = acc
          let #(pattern, options) = seg

          let #(size, inner_subject) = index_bit_array(options, subject, offset)

          let size = case size {
            -1 -> 0
            _ -> size
          }

          let offset = offset + size
          let new_binding = lower_pattern_bindings(c, pattern, inner_subject)
          #(offset, list.append(bindings, new_binding))
        })
      segs
    }
    t.PatternConstructor(
      typ,
      module,
      constructor,
      arguments,
      ordered_arguments,
      _,
      _,
    ) -> {
      let elems = ordered_arguments
      list.index_map(elems, fn(elem, i) {
        let subject =
          t.FieldAccess(elem.typ, subject, module, constructor, "", i)
        lower_pattern_bindings(c, elem, subject)
      })
      |> list.flatten
    }
  }
}

const nil_type = NamedType("Nil", [])

const bool_type = NamedType("Bool", [])

const int_type = NamedType("Int", [])

const true_value = Literal(bool_type, Bool("True"))

fn if_exp(cond: Exp, then: Exp, els: Exp) -> Exp {
  case cond {
    Literal(_, Bool("True")) -> then
    Literal(_, Bool("False")) -> els
    _ -> {
      If(then.typ, cond, then, els)
    }
  }
}

fn and_exp(first: Exp, second: Exp) {
  case first, second {
    Literal(_, Bool("True")), _ -> second
    _, Literal(_, Bool("True")) -> first
    _, _ -> {
      let subject = Local(first.typ, "B")
      let body = if_exp(subject, second, subject)
      Let(body.typ, "B", first, body)
    }
  }
}

fn or_exp(first: Exp, second: Exp) {
  case first, second {
    Literal(_, Bool("False")), _ -> second
    _, Literal(_, Bool("False")) -> first
    _, _ -> {
      let subject = Local(first.typ, "B")
      let body = if_exp(subject, subject, second)
      Let(body.typ, "B", first, body)
    }
  }
}

fn add_exp(first: Exp, second: Exp) {
  case first, second {
    Literal(_, Int("0")), _ -> second
    _, Literal(_, Int("0")) -> first
    _, _ -> {
      let fun_typ = FunctionType([int_type, int_type], int_type)
      let fun = Global(fun_typ, "add_int")
      let args = [first, second]
      Call(int_type, fun, args)
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
      // TODO check if the order of boolean expression is correct i.e. left to right
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
    t.PatternBitString(typ, segs) -> {
      let #(total_size, rest, data_match) =
        list.fold(segs, #(0, False, true_value), fn(acc, seg) {
          let #(offset, rest, match) = acc
          let #(pattern, options) = seg

          let #(size, inner_subject) = index_bit_array(options, subject, offset)

          let rest = case size {
            -1 -> True
            _ -> rest
          }
          let size = case size {
            -1 -> 0
            _ -> size
          }

          let offset = offset + size

          let seg_match = lower_pattern_match(c, pattern, inner_subject)
          let match = and_exp(seg_match, match)

          #(offset, rest, match)
        })

      let length_subject =
        t.Call(
          t.int_type,
          t.GlobalVariable(
            t.FunctionType([t.bit_array_type], t.int_type),
            t.builtin,
            "length_bit_array",
          ),
          [t.Field(None, subject)],
          [subject],
        )

      let length_match_op = case rest {
        True -> g.GtEqInt
        False -> g.Eq
      }
      let length_match =
        t.BinaryOperator(
          t.bool_type,
          length_match_op,
          length_subject,
          t.Int(t.int_type, int.to_string(total_size)),
        )
      let length_match = lower_expression(c, length_match)

      and_exp(length_match, data_match)
    }
    t.PatternConstructor(
      typ,
      module,
      constructor,
      arguments,
      ordered_arguments,
      _,
      _,
    ) -> {
      let typ = map_type(c, typ)
      let elems = ordered_arguments
      let match =
        list.index_fold(elems, true_value, fn(match, elem, i) {
          let subject =
            t.FieldAccess(elem.typ, subject, module, constructor, "", i)
          let elem_match = lower_pattern_match(c, elem, subject)
          and_exp(elem_match, match)
        })
      // TODO special case for single variant no need to check
      let isa_name =
        gen_names.get_variant_check_name(get_id(module, constructor))
      let isa_typ = FunctionType([typ], bool_type)
      let isa_ref = Global(isa_typ, isa_name)
      let isa_match = Call(bool_type, isa_ref, [lower_expression(c, subject)])
      and_exp(isa_match, match)
    }
  }
}

type OptionState(a) {
  OptionUnset
  OptionOpen(exp: a)
  OptionClosed(exp: a)
}

fn update_option(old: OptionState(a), new: OptionState(a)) {
  case old {
    OptionUnset -> new
    OptionOpen(_) ->
      case new {
        OptionClosed(_) -> new
        OptionOpen(_) -> new
        OptionUnset -> old
      }
    OptionClosed(_) ->
      case new {
        OptionClosed(_) -> panic as "option set twice"
        OptionOpen(_) -> old
        OptionUnset -> old
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
      let typ = map_type(c, typ)

      case module, name {
        // convert these to literals
        "gleam", "Nil" -> Literal(typ, NilVal)
        "gleam", "True" -> Literal(typ, Bool("True"))
        "gleam", "False" -> Literal(typ, Bool("False"))
        _, _ -> {
          Global(typ, get_id(module, name))
        }
      }
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
      // TODO register tuple constructor and such if it doesnt exist
      let typ = map_type(c, typ)
      let elements = list.map(elements, lower_expression(c, _))
      let element_types = list.map(elements, fn(e) { e.typ })
      let len = int.to_string(list.length(elements))
      let fun = Global(FunctionType(element_types, typ), "Tuple" <> len)
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
    t.RecordUpdate(typ, _, module, constructor, record, fields, ordered_fields) -> {
      // bind record to subject
      // for each field in variant either take the new field or default to field access on subject
      // and call the constructor with that
      let typ = map_type(c, typ)
      let subject = Local(typ, "subject")
      let constructor = gen_names.get_id(module, constructor)
      let fields =
        list.index_map(ordered_fields, fn(f, i) {
          case f {
            Ok(field) -> lower_expression(c, field)
            Error(field_type) -> {
              let getter_name = gen_names.get_getter_name(constructor, i)
              let field_type = map_type(c, field_type)
              let getter_typ = FunctionType([typ], field_type)
              Call(field_type, Global(getter_typ, getter_name), [subject])
            }
          }
        })
      let record = lower_expression(c, record)
      let field_types = list.map(fields, fn(x) { x.typ })
      let constructor_typ = FunctionType(field_types, typ)
      let body = Call(typ, Global(constructor_typ, constructor), fields)
      Let(body.typ, "subject", record, body)
    }
    t.FieldAccess(typ, container, module, variant, label, i) -> {
      let typ = map_type(c, typ)
      let container = lower_expression(c, container)
      let getter_name = gen_names.get_getter_name(get_id(module, variant), i)
      let getter_typ = FunctionType([container.typ], typ)
      let getter = Global(getter_typ, getter_name)
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
    t.BitString(typ, segs) -> {
      let typ = map_type(c, typ)

      let segs =
        list.map(segs, fn(seg) {
          let #(exp, options) = seg
          let exp = lower_expression(c, exp)
          let #(size) =
            list.fold(options, #(OptionUnset), fn(acc, option) {
              let #(size_option) = acc
              let #(size) = case option {
                t.BigOption -> todo
                t.BytesOption | t.BitsOption -> {
                  let fun_type = FunctionType([typ], int_type)
                  let fun = Global(fun_type, "length_bit_array")
                  let size = Call(int_type, fun, [exp])
                  let size_option = update_option(size_option, OptionOpen(size))
                  #(size_option)
                }
                t.FloatOption -> todo
                t.IntOption -> todo
                t.LittleOption -> todo
                t.NativeOption -> todo
                t.SignedOption -> todo
                t.SizeOption(size) -> {
                  let size = Literal(int_type, Int(int.to_string(size)))
                  let size_option =
                    update_option(size_option, OptionClosed(size))
                  #(size_option)
                }
                t.SizeValueOption(e) -> {
                  let size = lower_expression(c, e)
                  let size_option =
                    update_option(size_option, OptionClosed(size))
                  #(size_option)
                }
                t.UnitOption(_) -> todo
                t.UnsignedOption -> todo
                t.Utf16CodepointOption -> todo
                t.Utf16Option -> todo
                t.Utf32CodepointOption -> todo
                t.Utf32Option -> todo
                t.Utf8CodepointOption -> todo
                t.Utf8Option -> todo
              }
              #(size)
            })
          let size = case size {
            OptionClosed(x) -> x
            OptionOpen(x) -> x
            OptionUnset -> Literal(int_type, Int("8"))
          }
          #(exp, size)
        })
      let total_size =
        list.map(segs, fn(x) { x.1 })
        |> list.fold_right(Literal(int_type, Int("0")), add_exp)
      let body =
        list.fold_right(segs, Local(typ, "bit_array"), fn(exp, seg) {
          let #(seg_value, seg_size) = seg
          let target = Local(typ, "bit_array")
          let offset = Local(int_type, "offset")
          let write_call = case seg_value.typ {
            NamedType("Int", []) -> {
              let write_typ =
                FunctionType([int_type, typ, int_type, int_type], nil_type)
              let write_fun = Global(write_typ, "write_bit_array_int")
              Call(nil_type, write_fun, [seg_value, target, offset, seg_size])
            }
            NamedType("BitArray", []) -> {
              let write_typ =
                FunctionType([typ, typ, int_type, int_type], nil_type)
              let write_fun = Global(write_typ, "write_bit_array")
              Call(nil_type, write_fun, [seg_value, target, offset, seg_size])
            }
            _ -> panic as "unsupported segment type"
          }
          let new_offset = add_exp(Local(int_type, "offset"), seg_size)
          let update_offset = Let(typ, "offset", new_offset, exp)
          Let(update_offset.typ, "_", write_call, update_offset)
        })
      let body = Let(body.typ, "offset", Literal(int_type, Int("0")), body)
      let body =
        Let(body.typ, "bit_array", Literal(typ, BitArray("total_size")), body)
      Let(body.typ, "total_size", total_size, body)
    }
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
        Let(acc.typ, name, value, acc)
      })
    }
    t.BinaryOperator(typ, g.And, left, right) -> {
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      and_exp(left, right)
    }
    t.BinaryOperator(typ, g.Or, left, right) -> {
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      or_exp(left, right)
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
    Literal(typ, val) ->
      case val {
        BitArray(var) if var == replace ->
          case with {
            Local(_, x) -> Literal(typ, BitArray(x))
            _ -> panic as "invalid replacement"
          }
        _ -> Literal(typ, val)
      }
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
      let assert Ok(x) = dict.get(c.type_vars, ref)
      case x {
        t.Bound(x) -> map_type(c, x)
        t.Unbound(x) -> Unbound(x)
      }
    }
  }
}

fn unshadow(taken: List(String), i: Int, e: Exp) -> #(List(String), Int, Exp) {
  case e {
    Literal(_, _) -> #(taken, i, e)
    Local(_, _) -> #(taken, i, e)
    Global(_, _) -> #(taken, i, e)
    Call(typ, fun, args) -> {
      let #(taken, i, fun) = unshadow(taken, i, fun)
      let #(taken, i, args) =
        list.fold(args, #(taken, i, []), fn(acc, arg) {
          let #(taken, i, l) = acc
          let #(taken, i, arg) = unshadow(taken, i, arg)
          #(taken, i, [arg, ..l])
        })
      let args = list.reverse(args)
      #(taken, i, Call(typ, fun, args))
    }
    Fn(typ, vars, exp) -> {
      let #(taken, i, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var.name) {
            True -> {
              let new_var = var.name <> "V" <> int.to_string(i)
              let i = i + 1
              let exp = replace_var(var.name, Local(typ, new_var), exp)
              let taken = [new_var, ..taken]
              let #(taken, i, exp) = unshadow(taken, i, exp)
              let vars = [Parameter(var.typ, new_var), ..vars]
              #(taken, i, vars, exp)
            }
            False -> {
              let taken = [var.name, ..taken]
              let #(taken, i, exp) = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
          }
        })
      let #(taken, i, exp) = unshadow(taken, i, exp)
      let vars = list.reverse(vars)
      #(taken, i, Fn(typ, vars, exp))
    }
    Let(typ, var, val, exp) ->
      case list.contains(taken, var) {
        True -> {
          let new_var = var <> "V" <> int.to_string(i)
          let taken = [new_var, ..taken]
          let i = i + 1
          let #(taken, i, val) = unshadow(taken, i, val)
          let exp = replace_var(var, Local(val.typ, new_var), exp)
          let #(taken, i, exp) = unshadow(taken, i, exp)
          #(taken, i, Let(typ, new_var, val, exp))
        }
        False -> {
          let taken = [var, ..taken]
          let #(taken, i, val) = unshadow(taken, i, val)
          let #(taken, i, exp) = unshadow(taken, i, exp)
          #(taken, i, Let(typ, var, val, exp))
        }
      }
    If(typ, cond, then_exp, else_exp) -> {
      let #(taken, i, cond) = unshadow(taken, i, cond)
      let #(taken, i, then_exp) = unshadow(taken, i, then_exp)
      let #(taken, i, else_exp) = unshadow(taken, i, else_exp)
      #(taken, i, If(typ, cond, then_exp, else_exp))
    }
    Panic(typ, val) -> {
      let #(taken, i, val) = unshadow(taken, i, val)
      #(taken, i, Panic(typ, val))
    }
  }
}
