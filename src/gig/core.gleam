import gig/gen_names.{get_id}
import gig/typed_ast as t
import gleam/dict
import gleam/result
import gleam/string
import glexer
import listx

import glance as g

import gleam/int
import gleam/list
import gleam/option.{None, Some}

pub const builtin = t.builtin

pub type TypeVarId =
  t.TypeVarId

pub type Type {
  NamedType(id: String, parameters: List(Type))
  FunctionType(parameters: List(Type), return: Type)
  Unbound(id: TypeVarId)
}

pub type Poly {
  Poly(vars: List(TypeVarId), typ: Type)
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

pub type Op {
  FieldAccess(variant: String, field: String)
  VariantCheck(variant: String)
}

pub type Exp {
  Literal(typ: Type, value: LiteralKind)
  Local(typ: Type, name: String)
  Global(typ: Type, id: String)
  Fn(typ: Type, parameters: List(Parameter), body: Exp)
  Call(typ: Type, function: Exp, arguments: List(Exp))
  Op(typ: Type, op: Op, arguments: List(Exp))
  // TODO we could give every let binding a unique integer id
  // to solve any shadowing issues (maybe in typed_ast?)
  Let(typ: Type, name: String, value: Exp, body: Exp)
  If(typ: Type, condition: Exp, then: Exp, els: Exp)
  Panic(typ: Type, value: Exp)
}

pub type CustomType {
  CustomType(
    typ: Poly,
    id: String,
    display_name: String,
    variants: List(Variant),
  )
}

pub type Variant {
  // TODO bring back field names?
  Variant(typ: Poly, id: String, display_name: String, fields: List(Parameter))
}

pub type Function {
  Function(typ: Poly, id: String, parameters: List(Parameter), body: Exp)
}

pub type External {
  External(
    typ: Poly,
    module: String,
    internal_name: String,
    external_name: String,
    parameters: List(Parameter),
    builtin: Bool,
  )
}

pub type Context {
  Context(
    types: List(CustomType),
    functions: List(Function),
    externals: List(External),
  )
}

pub fn lower_context(c: t.Context) {
  let acc = Context(types: [], functions: [], externals: [])
  dict.values(c.modules)
  |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
  |> list.fold(acc, fn(acc, module) { lower_module(c, acc, module) })
}

fn lower_module(c: t.Context, acc: Context, module: t.Module) {
  let c = t.Context(..c, current_module: module.name)

  // TODO detect what tuples are actually used
  let acc =
    listx.sane_range(10)
    |> list.fold(acc, register_tuple)

  let acc =
    list.fold(module.custom_types, acc, fn(acc, custom) {
      case custom.definition.variants {
        // no variants is considered external so we don't register it
        [] -> acc
        _ -> {
          let custom = lower_custom_type(c, custom.definition)
          Context(..acc, types: [custom, ..acc.types])
        }
      }
    })

  // create type related builtin functions
  let acc =
    list.fold(acc.types, acc, fn(acc, custom) {
      list.fold(custom.variants, acc, fn(acc, variant) {
        register_variant_functions(acc, module.name, variant)
      })
    })

  let acc =
    list.fold(module.functions, acc, fn(acc, fun) {
      let attrs = fun.attributes
      let external =
        list.find(attrs, fn(x) {
          case x {
            t.Attribute("external", [t.LocalVariable(_, "c"), ..]) -> True
            _ -> False
          }
        })

      case external {
        Ok(external) -> {
          let assert t.Attribute(
            _,
            [
              t.LocalVariable(_, "c"),
              t.String(_, _src),
              t.String(_, external_name),
            ],
          ) = external
          let fun = fun.definition
          let typ = map_poly(fun.typ)
          let module = c.current_module
          let name = fun.name
          let internal_name = get_id(module, name)
          let builtin = list.any(attrs, fn(x) { x.name == "builtin" })
          let parameters = list.map(fun.parameters, lower_parameter)
          let fun =
            External(
              typ:,
              parameters:,
              internal_name:,
              external_name:,
              module:,
              builtin:,
            )
          Context(..acc, externals: [fun, ..acc.externals])
        }
        Error(_) -> {
          let fun = lower_function(c, fun)
          Context(..acc, functions: [fun, ..acc.functions])
        }
      }
    })
  acc
}

fn lower_custom_type(c: t.Context, custom: t.CustomType) {
  let typ = map_poly(custom.typ)
  let module = c.current_module
  let name = custom.name
  let id = get_id(module, name)
  let variants =
    list.map(custom.variants, fn(variant) {
      let typ = map_poly(variant.typ)
      let fields =
        list.index_map(variant.fields, fn(field, i) {
          let name = case field.label {
            Some(label) -> label
            None -> gen_names.get_field_name("", i)
          }
          Parameter(map_type(field.item.typ), name)
        })
      let id = get_id(module, variant.name)
      Variant(typ, id, variant.name, fields)
    })
  CustomType(typ, id, name, variants)
}

fn lower_function(c: t.Context, def: t.Definition(t.FunctionDefinition)) {
  let c = t.Context(..c, current_definition: def.definition.name)
  let function = def.definition
  let typ = map_poly(function.typ)
  let module = c.current_module
  let name = function.name
  let id = get_id(module, name)
  let parameters = list.map(function.parameters, lower_parameter)
  let body = lower_body(c, function.body)
  let taken = list.map(parameters, fn(param) { param.name })
  let #(_, _, body) = unshadow(taken, 1, body)

  Function(typ:, id:, parameters:, body:)
}

fn lower_parameter(parameter: t.FunctionParameter) {
  let typ = map_type(parameter.typ)
  let name = case parameter.name {
    t.Named(name) -> name
    t.Discarded(name) -> "_" <> name
  }
  Parameter(typ, name)
}

pub fn register_variant_functions(
  c: Context,
  module_name: String,
  variant: Variant,
) {
  // constructor function
  let params =
    list.map(variant.fields, fn(field) { Parameter(field.typ, field.name) })
  let fun =
    External(
      typ: variant.typ,
      internal_name: variant.id,
      external_name: "new_" <> variant.id,
      parameters: params,
      module: module_name,
      builtin: True,
    )
  Context(..c, externals: [fun, ..c.externals])
}

fn lower_body(c: t.Context, body: List(t.Statement)) {
  case body {
    [] -> lower_expression(c, t.Todo(t.nil_type, None))
    [statement] ->
      case statement {
        t.Expression(_, exp) -> lower_expression(c, exp)
        t.Assignment(_, kind, pattern, _, value) -> {
          let body = lower_expression(c, value)

          // check assertions
          case kind {
            t.LetAssert -> {
              let subject = t.LocalVariable(value.typ, "S")
              let value = lower_expression(c, value)
              let body = check_assertions(c, pattern, subject, value, body)
              Let(body.typ, "S", value, body)
            }
            t.Let -> body
          }
        }
        t.Assert(typ, expression, message) -> {
          let expression = lower_expression(c, expression)
          let body = lower_body(c, body)
          let message = lower_expression(c, t.Panic(typ, message))
          If(typ: body.typ, condition: expression, then: body, els: message)
        }
        t.Use(..) -> todo
      }
    [statement, ..body] ->
      case statement {
        t.Expression(_, value) -> {
          let value = lower_expression(c, value)
          let body = lower_body(c, body)
          Let(body.typ, "_", value, body)
        }
        t.Assignment(_, _, t.PatternVariable(_, name), _, value) -> {
          let value = lower_expression(c, value)
          let body = lower_body(c, body)
          Let(body.typ, name, value, body)
        }
        t.Assignment(_, kind, pattern, _, value) -> {
          let subject = t.LocalVariable(value.typ, "S")
          let value = lower_expression(c, value)
          let body = lower_body(c, body)

          let bindings = lower_pattern_bindings(pattern, subject)
          let body =
            list.fold(bindings, body, fn(body, binding) {
              let #(name, subject) = binding
              let value = lower_expression(c, subject)
              Let(body.typ, name, value, body)
            })

          // check assertions
          let body = case kind {
            t.LetAssert -> check_assertions(c, pattern, subject, value, body)
            t.Let -> body
          }

          Let(body.typ, "S", value, body)
        }
        t.Assert(typ, expression, message) -> {
          let expression = lower_expression(c, expression)
          let body = lower_body(c, body)
          let message = lower_expression(c, t.Panic(typ, message))
          If(typ: body.typ, condition: expression, then: body, els: message)
        }
        t.Use(..) -> todo
      }
  }
}

fn check_assertions(
  c: t.Context,
  pattern: t.Pattern,
  subject: t.Expression,
  value: Exp,
  body: Exp,
) -> Exp {
  let match = lower_pattern_match(c, pattern, subject)
  let message = "Assertion failed in " <> current_location(c) <> "\n"
  let inspect_typ = FunctionType([value.typ], string_type)
  let append_typ = FunctionType([string_type, string_type], string_type)
  let m =
    Call(string_type, Global(append_typ, "append_string"), [
      Literal(string_type, String(message)),
      Call(string_type, Global(inspect_typ, "inspect"), [Local(value.typ, "S")]),
    ])
  let els = Panic(body.typ, m)
  If(body.typ, match, body, els)
}

type BitArrayMode {
  IntMode
  FloatMode
  Utf8CodepointMode
  Utf16CodepointMode
  Utf32CodepointMode
  BitsMode
  BytesMode
  Utf8Mode
  Utf16Mode
  Utf32Mode
}

type Signedness {
  Signed
  Unsigned
}

type Endianness {
  BigEndian
  LittleEndian
  NativeEndian
}

type Sizedness {
  Sized(e: t.Expression)
  Unsized
}

fn parse_bitstring_segment_expression(
  value: t.Expression,
  options: List(t.BitStringSegmentOption(t.Expression)),
) {
  let mode =
    list.find_map(options, fn(option) {
      case option {
        t.IntOption -> Ok(IntMode)
        t.FloatOption -> Ok(FloatMode)
        t.Utf8CodepointOption -> Ok(Utf8CodepointMode)
        t.Utf16CodepointOption -> Ok(Utf16CodepointMode)
        t.Utf32CodepointOption -> Ok(Utf32CodepointMode)
        t.BitsOption -> Ok(BitsMode)
        t.BytesOption -> Ok(BytesMode)
        t.Utf8Option -> Ok(Utf8Mode)
        t.Utf16Option -> Ok(Utf16Mode)
        t.Utf32Option -> Ok(Utf32Mode)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case value {
      t.Int(..) -> IntMode
      t.Float(..) -> FloatMode
      t.String(..) -> Utf8Mode
      _ -> IntMode
    })

  let size =
    list.find_map(options, fn(option) {
      case option {
        t.SizeOption(size) -> Ok(Sized(t.Int(t.int_type, int.to_string(size))))
        t.SizeValueOption(size) -> Ok(Sized(size))
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case mode {
      IntMode -> Sized(t.Int(t.int_type, "8"))
      FloatMode -> Sized(t.Int(t.int_type, "64"))
      Utf8CodepointMode -> Unsized
      Utf16CodepointMode -> Unsized
      Utf32CodepointMode -> Unsized
      BitsMode -> Unsized
      BytesMode -> Unsized
      Utf8Mode -> Unsized
      Utf16Mode -> Unsized
      Utf32Mode -> Unsized
    })

  let unit =
    list.find_map(options, fn(option) {
      case option {
        t.UnitOption(value) -> Ok(value)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case mode {
      BytesMode -> 8
      _ -> 1
    })

  let signed =
    list.find_map(options, fn(option) {
      case option {
        t.SignedOption -> Ok(Signed)
        t.UnsignedOption -> Ok(Unsigned)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(Unsigned)

  let endian =
    list.find_map(options, fn(option) {
      case option {
        t.BigOption -> Ok(BigEndian)
        t.LittleOption -> Ok(LittleEndian)
        t.NativeOption -> Ok(NativeEndian)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(BigEndian)

  #(mode, size, unit, signed, endian)
}

fn parse_bitstring_segment_pattern(
  value: t.Pattern,
  options: List(t.BitStringSegmentOption(t.Pattern)),
) {
  let mode =
    list.find_map(options, fn(option) {
      case option {
        t.IntOption -> Ok(IntMode)
        t.FloatOption -> Ok(FloatMode)
        t.Utf8CodepointOption -> Ok(Utf8CodepointMode)
        t.Utf16CodepointOption -> Ok(Utf16CodepointMode)
        t.Utf32CodepointOption -> Ok(Utf32CodepointMode)
        t.BitsOption -> Ok(BitsMode)
        t.BytesOption -> Ok(BytesMode)
        t.Utf8Option -> Ok(Utf8Mode)
        t.Utf16Option -> Ok(Utf16Mode)
        t.Utf32Option -> Ok(Utf32Mode)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case value {
      t.PatternInt(..) -> IntMode
      t.PatternFloat(..) -> FloatMode
      t.PatternString(..) -> Utf8Mode
      _ -> IntMode
    })

  let size =
    list.find_map(options, fn(option) {
      case option {
        t.SizeOption(size) -> Ok(Sized(t.Int(t.int_type, int.to_string(size))))
        t.SizeValueOption(pattern) ->
          case pattern {
            t.PatternInt(typ, value) -> Ok(Sized(t.Int(typ, value)))
            t.PatternVariable(typ, value) ->
              Ok(Sized(t.LocalVariable(typ, value)))
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case mode {
      IntMode -> Sized(t.Int(t.int_type, "8"))
      FloatMode -> Sized(t.Int(t.int_type, "64"))
      Utf8CodepointMode -> Unsized
      Utf16CodepointMode -> Unsized
      Utf32CodepointMode -> Unsized
      BitsMode -> Unsized
      BytesMode -> Unsized
      Utf8Mode -> Unsized
      Utf16Mode -> Unsized
      Utf32Mode -> Unsized
    })

  let unit =
    list.find_map(options, fn(option) {
      case option {
        t.UnitOption(value) -> Ok(value)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case mode {
      BytesMode -> 8
      _ -> 1
    })

  let signed =
    list.find_map(options, fn(option) {
      case option {
        t.SignedOption -> Ok(Signed)
        t.UnsignedOption -> Ok(Unsigned)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(Unsigned)

  let endian =
    list.find_map(options, fn(option) {
      case option {
        t.BigOption -> Ok(BigEndian)
        t.LittleOption -> Ok(LittleEndian)
        t.NativeOption -> Ok(NativeEndian)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(BigEndian)

  #(mode, size, unit, signed, endian)
}

fn endian_expression(endian: Endianness) -> t.Expression {
  case endian {
    NativeEndian -> t.Int(t.int_type, "0")
    BigEndian -> t.Int(t.int_type, "1")
    LittleEndian -> t.Int(t.int_type, "2")
  }
}

fn index_bit_array(
  options: List(t.BitStringSegmentOption(t.Pattern)),
  subject: t.Expression,
  offset: t.Expression,
  expr: t.Pattern,
) -> #(t.Expression, Bool, t.Expression) {
  let #(mode, size_value, unit, signed, endian) =
    parse_bitstring_segment_pattern(expr, options)

  case mode {
    BitsMode | BytesMode -> {
      let #(size, match_to_end) = case size_value {
        Sized(size) -> {
          let size = case unit {
            1 -> size
            _ ->
              t.BinaryOperator(
                t.int_type,
                g.MultInt,
                size,
                t.Int(t.int_type, int.to_string(unit)),
              )
          }
          #(size, False)
        }
        Unsized -> #(t.Int(t.int_type, "-1"), True)
      }

      let inner_subject =
        t.Call(
          t.bit_array_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.bit_array_type,
            ),
            t.builtin,
            "slice_bit_array",
            [],
          ),
          [subject, offset, size],
        )

      #(size, match_to_end, inner_subject)
    }

    IntMode -> {
      let size = case size_value {
        Sized(size) -> size
        Unsized -> t.Int(t.int_type, "8")
      }

      let size = case unit {
        1 -> size
        _ ->
          t.BinaryOperator(
            t.int_type,
            g.MultInt,
            size,
            t.Int(t.int_type, int.to_string(unit)),
          )
      }

      // Choose the appropriate function based on signedness and endianness
      let function_name = case signed {
        Unsigned -> "index_bit_array_int"
        Signed -> "index_bit_array_int_signed"
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.int_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type, t.int_type],
              t.int_type,
            ),
            t.builtin,
            function_name,
            [],
          ),
          [subject, offset, size, endian],
        )

      #(size, False, inner_subject)
    }

    FloatMode -> {
      let size = case size_value {
        Sized(size) -> size
        Unsized -> t.Int(t.int_type, "64")
      }

      let size = case unit {
        1 -> size
        _ ->
          t.BinaryOperator(
            t.int_type,
            g.MultInt,
            size,
            t.Int(t.int_type, int.to_string(unit)),
          )
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.float_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type, t.int_type],
              t.float_type,
            ),
            t.builtin,
            "index_bit_array_float",
            [],
          ),
          [subject, offset, size, endian],
        )

      #(size, False, inner_subject)
    }

    Utf8Mode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported"
        Unsized -> {
          let assert t.PatternString(_, value) = expr
          let assert Ok(unescaped) = glexer.unescape_string(value)
          let size = string.byte_size(unescaped)
          t.Int(t.int_type, int.to_string(8 * size))
        }
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.string_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type, t.int_type],
              t.string_type,
            ),
            t.builtin,
            "index_bit_array_utf8_string",
            [],
          ),
          [subject, offset, size, endian],
        )

      #(size, False, inner_subject)
    }

    Utf16Mode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported for UTF-16"
        Unsized -> {
          let assert t.PatternString(_, value) = expr
          let fun_type = t.FunctionType([t.string_type], t.int_type)
          let fun = t.Function(fun_type, t.builtin, "utf16_string_bit_size", [])
          t.Call(t.int_type, fun, [t.String(t.string_type, value)])
        }
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.string_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type, t.int_type],
              t.string_type,
            ),
            t.builtin,
            "index_bit_array_utf16_string",
            [],
          ),
          [subject, offset, size, endian],
        )

      #(size, False, inner_subject)
    }

    Utf32Mode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported for UTF-32"
        Unsized -> {
          let assert t.PatternString(_, value) = expr
          let fun_type = t.FunctionType([t.string_type], t.int_type)
          let fun = t.Function(fun_type, t.builtin, "utf32_string_bit_size", [])
          t.Call(t.int_type, fun, [t.String(t.string_type, value)])
        }
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.string_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type, t.int_type],
              t.string_type,
            ),
            t.builtin,
            "index_bit_array_utf32_string",
            [],
          ),
          [subject, offset, size, endian],
        )

      #(size, False, inner_subject)
    }

    Utf8CodepointMode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported for UTF-8 codepoint"
        Unsized -> t.Int(t.int_type, "8")
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.codepoint_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.codepoint_type,
            ),
            t.builtin,
            "index_bit_array_utf8",
            [],
          ),
          [subject, offset, endian],
        )

      #(size, False, inner_subject)
    }

    Utf16CodepointMode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported for UTF-16 codepoint"
        Unsized -> t.Int(t.int_type, "16")
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.codepoint_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.codepoint_type,
            ),
            t.builtin,
            "index_bit_array_utf16",
            [],
          ),
          [subject, offset, endian],
        )

      #(size, False, inner_subject)
    }

    Utf32CodepointMode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported for UTF-32 codepoint"
        Unsized -> t.Int(t.int_type, "32")
      }

      let endian = endian_expression(endian)

      let inner_subject =
        t.Call(
          t.codepoint_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.codepoint_type,
            ),
            t.builtin,
            "index_bit_array_utf32",
            [],
          ),
          [subject, offset, endian],
        )

      #(size, False, inner_subject)
    }
  }
}

fn lower_pattern_bindings(
  pattern: t.Pattern,
  subject: t.Expression,
) -> List(#(String, t.Expression)) {
  case pattern {
    t.PatternInt(..) -> []
    t.PatternFloat(..) -> []
    t.PatternString(..) -> []
    t.PatternDiscard(..) -> []
    t.PatternVariable(name:, ..) -> [#(name, subject)]
    t.PatternTuple(elems:, ..) -> {
      list.index_map(elems, fn(elem, i) {
        let subject = t.TupleIndex(elem.typ, subject, i)
        lower_pattern_bindings(elem, subject)
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
          let a = [t.Field(Some("item"), x), t.Field(Some("next"), rest)]
          t.PatternConstructor(typ, t.builtin, "Cons", a, a, True, False)
        })
      lower_pattern_bindings(list, subject)
    }
    t.PatternAssignment(pattern:, name:, ..) -> {
      let pattern = lower_pattern_bindings(pattern, subject)
      [#(name, subject), ..pattern]
    }
    t.PatternConcatenate(prefix:, prefix_name:, suffix_name:, ..) -> {
      let prefix_binding = case prefix_name {
        Some(t.Named(name)) -> [#(name, t.String(t.string_type, prefix))]
        _ -> []
      }

      let suffix_binding = case suffix_name {
        t.Named(name) -> {
          // Create a call to drop the prefix from the subject string
          // We have to unescape the prefix to get the real size
          let assert Ok(unescaped_prefix) = glexer.unescape_string(prefix)
          let size = string.byte_size(unescaped_prefix)
          let prefix_len = t.Int(t.int_type, int.to_string(size))
          let drop_fun =
            t.Function(
              t.FunctionType([t.string_type, t.int_type], t.string_type),
              t.builtin,
              "drop_start_string",
              [],
            )
          let suffix_value =
            t.Call(t.string_type, drop_fun, [subject, prefix_len])
          [#(name, suffix_value)]
        }
        t.Discarded(_) -> []
      }

      list.append(prefix_binding, suffix_binding)
    }
    t.PatternBitString(segments:, ..) -> {
      // TODO total size not used? can we remove the calculation?
      let #(total_size, segs) =
        list.fold(segments, #(t.Int(t.int_type, "0"), []), fn(acc, seg) {
          let #(offset, bindings) = acc
          let #(pattern, options) = seg

          let #(size, _match_to_end, inner_subject) =
            index_bit_array(options, subject, offset, pattern)

          let offset = t.BinaryOperator(t.int_type, g.AddInt, offset, size)
          let new_binding = lower_pattern_bindings(pattern, inner_subject)
          #(offset, list.append(bindings, new_binding))
        })
      segs
      |> list.reverse()
    }
    t.PatternConstructor(module:, constructor:, ordered_arguments:, ..) -> {
      let elems = ordered_arguments
      list.index_map(elems, fn(elem, index) {
        let label = option.unwrap(elem.label, "")
        let subject =
          t.FieldAccess(
            elem.item.typ,
            subject,
            module,
            constructor,
            label,
            index,
          )
        lower_pattern_bindings(elem.item, subject)
      })
      |> list.flatten
    }
  }
}

pub const nil_type = NamedType("Nil", [])

pub const bool_type = NamedType("Bool", [])

pub const int_type = NamedType("Int", [])

pub const string_type = NamedType("String", [])

pub const true_value = Literal(bool_type, Bool("True"))

pub const false_value = Literal(bool_type, Bool("False"))

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
    _, _ -> if_exp(first, second, false_value)
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
    t.PatternDiscard(_typ, _name) -> true_value
    t.PatternVariable(_typ, _name) -> true_value
    t.PatternTuple(_typ, elems) -> {
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
          let a = [t.Field(Some("item"), x), t.Field(Some("next"), rest)]
          t.PatternConstructor(typ, t.builtin, "Cons", a, a, True, False)
        })
      lower_pattern_match(c, list, subject)
    }
    t.PatternAssignment(_typ, pattern, _name) -> {
      lower_pattern_match(c, pattern, subject)
    }
    t.PatternConcatenate(_typ, prefix, _prefix_name, _suffix_name) -> {
      let prefix_str = t.String(t.string_type, prefix)
      let match =
        t.Function(
          t.FunctionType([t.string_type, t.string_type], t.bool_type),
          t.builtin,
          "starts_with_string",
          [],
        )
      let starts_with_call = t.Call(t.bool_type, match, [subject, prefix_str])
      lower_expression(c, starts_with_call)
    }
    t.PatternBitString(_typ, segs) -> {
      let #(total_size, match_to_end, data_match) =
        list.fold(
          segs,
          #(t.Int(t.int_type, "0"), False, true_value),
          fn(acc, seg) {
            let #(offset, match_to_end, match) = acc
            let #(pattern, options) = seg

            case match_to_end {
              True -> panic as "can not match after matching to end"
              False -> Nil
            }

            let #(size, match_to_end, inner_subject) =
              index_bit_array(options, subject, offset, pattern)

            let offset = t.BinaryOperator(t.int_type, g.AddInt, offset, size)

            let seg_match = lower_pattern_match(c, pattern, inner_subject)
            let match = and_exp(seg_match, match)

            #(offset, match_to_end, match)
          },
        )

      let length_subject =
        t.Call(
          t.int_type,
          t.Function(
            t.FunctionType([t.bit_array_type], t.int_type),
            t.builtin,
            "length_bit_array",
            [],
          ),
          [subject],
        )

      let length_match_op = case match_to_end {
        True -> g.GtEqInt
        False -> g.Eq
      }

      let length_match =
        t.BinaryOperator(
          t.bool_type,
          length_match_op,
          length_subject,
          total_size,
        )
      let length_match = lower_expression(c, length_match)

      and_exp(length_match, data_match)
    }
    t.PatternConstructor(
      _typ,
      module,
      constructor,
      _arguments,
      ordered_arguments,
      _,
      _,
    ) -> {
      case module, constructor {
        // handle special cases
        "gleam", "Nil" -> true_value
        "gleam", "True" -> lower_expression(c, subject)
        "gleam", "False" -> {
          let not_subject = t.NegateBool(t.bool_type, subject)
          lower_expression(c, not_subject)
        }
        _, _ -> {
          let elems = ordered_arguments
          let match =
            list.index_fold(elems, true_value, fn(match, elem, index) {
              let label = option.unwrap(elem.label, "")
              let subject =
                t.FieldAccess(
                  elem.item.typ,
                  subject,
                  module,
                  constructor,
                  label,
                  index,
                )
              let elem_match = lower_pattern_match(c, elem.item, subject)
              and_exp(elem_match, match)
            })

          let subject = lower_expression(c, subject)
          let assert NamedType(custom, _) = subject.typ
          let assert Ok(mod) = dict.get(c.modules, module)
          let assert Ok(t.Definition(_, custom)) =
            list.find(mod.custom_types, fn(c) {
              get_id(module, c.definition.name) == custom
            })
          let variant = get_id(module, constructor)

          let variant_match = case custom.variants {
            [_] -> true_value
            _ -> Op(bool_type, VariantCheck(variant), [subject])
          }

          and_exp(variant_match, match)
        }
      }
    }
  }
}

fn lower_expression(c: t.Context, exp: t.Expression) -> Exp {
  case exp {
    t.Int(typ, value) -> Literal(map_type(typ), Int(value))
    t.Float(typ, value) -> Literal(map_type(typ), Float(value))
    t.String(typ, value) -> Literal(map_type(typ), String(value))
    t.LocalVariable(typ, name) -> Local(map_type(typ), name)
    t.Function(typ, module, name, _labels) -> {
      let typ = map_type(typ)

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
    t.Constant(value:, ..) -> {
      // inline the constant
      lower_expression(c, value)
    }
    t.NegateInt(typ, value) -> {
      let typ = map_type(typ)
      let value = lower_expression(c, value)
      let fun = Global(FunctionType([typ], typ), "negate_int")
      Call(typ, fun, [value])
    }
    t.NegateBool(typ, value) -> {
      let typ = map_type(typ)
      let value = lower_expression(c, value)
      let fun = Global(FunctionType([typ], typ), "negate_bool")
      Call(typ, fun, [value])
    }
    t.Block(_typ, statements) -> lower_body(c, statements)
    t.Panic(typ, value) -> {
      let typ = map_type(typ)
      let value = case value {
        Some(value) -> lower_expression(c, value)
        None -> {
          let message = "panic: " <> current_location(c)
          Literal(map_type(t.string_type), String(message))
        }
      }
      Panic(typ, value)
    }
    t.Todo(typ, value) -> {
      let typ = map_type(typ)
      let value = case value {
        Some(value) -> lower_expression(c, value)
        None -> {
          let message = "todo: " <> current_location(c)
          Literal(map_type(t.string_type), String(message))
        }
      }
      Panic(typ, value)
    }
    t.Tuple(typ, elements) -> {
      let typ = map_type(typ)
      let elements = list.map(elements, lower_expression(c, _))
      let element_types = list.map(elements, fn(e) { e.typ })
      let len = int.to_string(list.length(elements))
      let fun = Global(FunctionType(element_types, typ), "Tuple" <> len)
      Call(typ, fun, elements)
    }
    t.List(typ, elements, rest) -> {
      let list_typ = map_type(typ)
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
    t.Fn(typ, parameters, _return, body) -> {
      let typ = map_type(typ)
      let parameters = list.map(parameters, lower_parameter)
      let body = lower_body(c, body)
      Fn(typ, parameters, body)
    }
    t.RecordUpdate(typ, _, module, constructor, record, _fields, ordered_fields) -> {
      // bind record to subject
      // for each field in variant either take the new field or default to field access on subject
      // and call the constructor with that
      let subject_name = case record {
        t.LocalVariable(_, name) -> name
        _ -> "S"
      }
      let subject = t.LocalVariable(typ, subject_name)
      let fields =
        list.index_map(ordered_fields, fn(f, i) {
          case f {
            Ok(field) -> lower_expression(c, field.item)
            Error(field_type) -> {
              let access =
                t.FieldAccess(field_type, subject, module, constructor, "", i)
              lower_expression(c, access)
            }
          }
        })
      let typ = map_type(typ)
      let record = lower_expression(c, record)
      let field_types = list.map(fields, fn(x) { x.typ })
      let constructor_typ = FunctionType(field_types, typ)
      let constructor = gen_names.get_id(module, constructor)
      let body = Call(typ, Global(constructor_typ, constructor), fields)
      Let(body.typ, subject_name, record, body)
    }
    t.FieldAccess(typ, container, module, variant, _label, index) -> {
      let typ = map_type(typ)
      let container = lower_expression(c, container)
      let assert NamedType(custom, _) = container.typ
      let assert Ok(module) = dict.get(c.modules, module)
      let assert Ok(t.Definition(_, custom)) =
        list.find(module.custom_types, fn(c) {
          get_id(module.name, c.definition.name) == custom
        })
      let assert Ok(variant) =
        list.find(custom.variants, fn(v) { v.name == variant })
      let assert [field, ..] = list.drop(variant.fields, index)
      let field = option.unwrap(field.label, "")
      let field = gen_names.get_field_name(field, index)
      Op(typ, FieldAccess(variant.name, field), [container])
    }
    t.Call(typ, function, args) -> {
      let typ = map_type(typ)
      let function = lower_expression(c, function)
      let arguments = list.map(args, lower_expression(c, _))
      Call(typ, function, arguments)
    }
    t.TupleIndex(typ, tuple, index) -> {
      let typ = map_type(typ)
      let tuple = lower_expression(c, tuple)
      Op(typ, FieldAccess("#", gen_names.get_field_name("", index)), [tuple])
    }
    t.FnCapture(..) -> todo
    t.BitString(typ, segs) -> {
      let typ = map_type(typ)

      let segs =
        list.map(segs, fn(seg) {
          let #(exp, options) = seg

          let #(mode, size_value, unit, signed, endian) =
            parse_bitstring_segment_expression(exp, options)
          let exp = lower_expression(c, exp)

          let size = case size_value {
            Sized(e) -> lower_expression(c, e)
            Unsized ->
              case mode {
                IntMode -> Literal(int_type, Int("8"))
                BitsMode | BytesMode -> {
                  let fun_type = FunctionType([typ], int_type)
                  let fun = Global(fun_type, "length_bit_array")
                  Call(int_type, fun, [exp])
                }
                FloatMode -> Literal(int_type, Int("64"))
                Utf8Mode -> {
                  let fun_type = FunctionType([string_type], int_type)
                  let fun = Global(fun_type, "utf8_string_bit_size")
                  Call(int_type, fun, [exp])
                }
                Utf16Mode -> {
                  let fun_type = FunctionType([string_type], int_type)
                  let fun = Global(fun_type, "utf16_string_bit_size")
                  Call(int_type, fun, [exp])
                }
                Utf32Mode -> {
                  let fun_type = FunctionType([string_type], int_type)
                  let fun = Global(fun_type, "utf32_string_bit_size")
                  Call(int_type, fun, [exp])
                }
                Utf8CodepointMode -> Literal(int_type, Int("8"))
                Utf16CodepointMode -> Literal(int_type, Int("16"))
                Utf32CodepointMode -> Literal(int_type, Int("32"))
              }
          }

          // Scale the size by the unit
          let size = case unit {
            1 -> size
            _ -> {
              let unit_size = Literal(int_type, Int(int.to_string(unit)))
              let mul_typ = FunctionType([int_type, int_type], int_type)
              let mul = Global(mul_typ, "mul_int")
              Call(int_type, mul, [size, unit_size])
            }
          }

          #(exp, size, mode, signed, endian)
        })

      let total_size =
        list.map(segs, fn(x) { x.1 })
        |> list.fold_right(Literal(int_type, Int("0")), add_exp)

      let body =
        list.fold_right(segs, Local(typ, "bit_array"), fn(exp, seg) {
          let #(seg_value, seg_size, mode, signed, endian) = seg
          let target = Local(typ, "bit_array")
          let offset = Local(int_type, "offset")

          let write_fun_name = case mode {
            IntMode -> "write_bit_array_int"
            FloatMode -> "write_bit_array_float"

            // TODO codepoint writing
            Utf8CodepointMode -> "write_bit_array_int"
            Utf16CodepointMode -> "write_bit_array_int"
            Utf32CodepointMode -> "write_bit_array_int"

            BitsMode -> "write_bit_array"
            BytesMode -> "write_bit_array"

            Utf8Mode -> "write_bit_array_utf8_string"
            Utf16Mode -> "write_bit_array_utf16_string"
            Utf32Mode -> "write_bit_array_utf32_string"
          }

          let #(endian, endian_type) = case mode {
            BitsMode | BytesMode -> #([], [])
            _ -> #([lower_expression(c, endian_expression(endian))], [int_type])
          }

          let write_typ =
            FunctionType(
              [seg_value.typ, typ, int_type, int_type, ..endian_type],
              nil_type,
            )
          let write_fun = Global(write_typ, write_fun_name)
          let write_call =
            Call(nil_type, write_fun, [
              seg_value,
              target,
              offset,
              seg_size,
              ..endian
            ])

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
      let typ = map_type(typ)
      let message = String("No matching clause in " <> current_location(c))
      let else_body = Panic(typ, Literal(map_type(t.string_type), message))

      // Create bindings for each subject
      let subject_vars =
        list.index_map(subjects, fn(subject, index) {
          // TODO special case for when subject is already a local
          let name = "S" <> int.to_string(index)
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
                lower_pattern_bindings(pattern, sub)
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
                [guard, ..conditions]
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
    t.BinaryOperator(_typ, g.And, left, right) -> {
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      and_exp(left, right)
    }
    t.BinaryOperator(_typ, g.Or, left, right) -> {
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      or_exp(left, right)
    }
    t.BinaryOperator(typ, g.NotEq, left, right) -> {
      let not_eq = t.NegateBool(typ, t.BinaryOperator(typ, g.Eq, left, right))
      lower_expression(c, not_eq)
    }
    t.BinaryOperator(typ, name, left, right) -> {
      let typ = map_type(typ)
      let left = lower_expression(c, left)
      let right = lower_expression(c, right)
      let function_name = case name {
        g.And -> panic as "and_bool"
        g.Or -> panic as "or_bool"
        g.Eq -> "eq"
        g.NotEq -> panic as "not_eq"
        g.LtInt -> "lt_int"
        g.LtEqInt -> "lte_int"
        g.LtFloat -> "lt_float"
        g.LtEqFloat -> "lte_float"
        g.GtEqInt -> "gte_int"
        g.GtInt -> "gt_int"
        g.GtEqFloat -> "gte_float"
        g.GtFloat -> "gt_float"
        g.Pipe -> panic as "pipe"
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
    t.Echo(typ:, value:) -> {
      let assert Some(value) = value
      let value = lower_expression(c, value)
      let typ = map_type(typ)
      let print_typ = FunctionType([value.typ], value.typ)
      Call(typ, Global(print_typ, "echo_"), [value])
    }
  }
}

fn current_location(c: t.Context) {
  c.current_module <> "." <> c.current_definition
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
    Op(typ, op, args) -> {
      let args = list.map(args, replace_var(replace, with, _))
      Op(typ, op, args)
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
    Panic(typ, arg) -> Panic(typ, replace_var(replace, with, arg))
  }
}

fn map_poly(typ: t.Poly) {
  Poly(typ.vars, map_type(typ.typ))
}

fn register_tuple(c: Context, size: Int) {
  let id = gen_names.get_tuple_id(size)
  let vars = list.map(listx.sane_range(size), t.TypeVarId("", _))
  let element_types = list.map(vars, fn(i) { Unbound(i) })
  let custom_typ = Poly(vars, NamedType(id, element_types))
  let constructor_typ = Poly(vars, FunctionType(element_types, custom_typ.typ))

  let element_fields =
    list.index_map(element_types, fn(typ, i) {
      Parameter(typ, gen_names.get_field_name("", i))
    })

  let variant = Variant(constructor_typ, id, "#", element_fields)
  let custom = CustomType(custom_typ, id, "#", [variant])

  let c = Context(..c, types: [custom, ..c.types])
  register_variant_functions(c, t.builtin, variant)
}

fn map_type(typ: t.Type) {
  case typ {
    t.NamedType(name:, module:, parameters:) -> {
      let parameters = list.map(parameters, map_type)
      NamedType(get_id(module, name), parameters)
    }
    t.FunctionType(parameters, return) -> {
      let parameters = list.map(parameters, map_type)
      let return = map_type(return)
      FunctionType(parameters, return)
    }
    t.TupleType(elements) -> {
      let parameters = list.map(elements, map_type)
      NamedType(gen_names.get_tuple_id(list.length(elements)), parameters)
    }
    t.VariableType(ref) -> {
      Unbound(ref)
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
    Op(typ, op, args) -> {
      let #(taken, i, args) =
        list.fold(args, #(taken, i, []), fn(acc, arg) {
          let #(taken, i, l) = acc
          let #(taken, i, arg) = unshadow(taken, i, arg)
          #(taken, i, [arg, ..l])
        })
      let args = list.reverse(args)
      #(taken, i, Op(typ, op, args))
    }
    Fn(typ, vars, exp) -> {
      let #(taken, i, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var.name) {
            True -> {
              let new_var = var.name <> "V" <> int.to_string(i)
              let vars = [Parameter(var.typ, new_var), ..vars]
              let exp = replace_var(var.name, Local(var.typ, new_var), exp)
              #(taken, i + 1, vars, exp)
            }
            False -> {
              let taken = [var.name, ..taken]
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
          let #(taken, i, val) = unshadow(taken, i + 1, val)
          let #(taken, i, exp) = unshadow(taken, i, exp)
          let exp = replace_var(var, Local(val.typ, new_var), exp)
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
    Panic(typ, arg) -> {
      let #(taken, i, arg) = unshadow(taken, i, arg)
      #(taken, i, Panic(typ, arg))
    }
  }
}
