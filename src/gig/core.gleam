import gig/gen_names.{get_id}
import gig/typed_ast as t
import gleam/dict.{type Dict}
import gleam/io
import gleam/result
import gleam/string
import glexer
import listx

import glance as g

import gleam/int
import gleam/list
import gleam/option.{None, Some}

pub const builtin = t.builtin

pub type Type {
  NamedType(id: String, parameters: List(Type))
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

pub type Op {
  FieldAccess(variant: String, index: Int)
  Panic
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
  External(typ: Poly, src: String, module: String, id: String, mono: Bool)
}

pub type Context {
  Context(
    types: Dict(String, CustomType),
    functions: Dict(String, Function),
    externals: Dict(String, External),
  )
}

pub fn lower_context(c: t.Context) {
  // these need registered because they are converted to lieterals
  let bool_poly = Poly([], bool_type)
  let nil_constructor =
    External(Poly([], nil_type), "", t.builtin, "Nil", False)
  let true_constructor = External(bool_poly, "", t.builtin, "True", False)
  let false_constructor = External(bool_poly, "", t.builtin, "False", False)
  let externals =
    dict.new()
    |> dict.insert("Nil", nil_constructor)
    |> dict.insert("True", true_constructor)
    |> dict.insert("False", false_constructor)

  let acc = Context(types: dict.new(), functions: dict.new(), externals:)
  dict.fold(c.modules, acc, fn(acc, name, module) {
    lower_module(c, acc, module)
  })
}

fn lower_module(c: t.Context, acc: Context, module: t.Module) {
  let c = t.Context(..c, current_module: module.name)

  // remove some types that are replaced with builtins
  let #(acc, custom_types) = case module.name == t.builtin {
    True -> {
      let #(normal_types, special_types) =
        list.partition(module.custom_types, fn(c) {
          case c.definition.name {
            "Nil" | "Bool" -> False
            _ -> True
          }
        })

      let acc =
        list.fold(special_types, acc, fn(acc, custom) {
          let custom = lower_custom_type(c, custom.definition)
          list.fold(custom.variants, acc, fn(acc, variant) {
            register_variant_functions(acc, module.name, custom.typ, variant)
          })
        })

      #(acc, normal_types)
    }
    False -> #(acc, module.custom_types)
  }

  // TODO detect what tuples are actually used
  let acc =
    listx.sane_range(10)
    |> list.fold(acc, register_tuple)

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

  // create type related builtin functions
  let acc =
    list.fold(dict.values(acc.types), acc, fn(acc, custom) {
      list.fold(custom.variants, acc, fn(acc, variant) {
        register_variant_functions(acc, module.name, custom.typ, variant)
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
              t.String(_, src),
              t.String(_, external_id),
            ],
          ) = external
          let typ = map_poly(c, fun.definition.typ)
          let module = c.current_module
          let name = fun.definition.name
          let internal_id = get_id(module, name)
          let mono = list.any(attrs, fn(x) { x.name == "monomorphise" })
          // TODO actual src file?
          let src = module
          let fun = External(typ, src, module, external_id, mono)
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
        list.index_map(variant.fields, fn(field, i) {
          let name = case field.label {
            Some(label) -> label
            None -> gen_names.get_field_name("", i)
          }
          Parameter(map_type(c, field.item.typ), name)
        })
      let id = get_id(module, variant.name)
      Variant(typ, id, variant.name, fields)
    })
  CustomType(typ, id, name, variants)
}

fn lower_function(c: t.Context, def: t.Definition(t.FunctionDefinition)) {
  let c = t.Context(..c, current_definition: def.definition.name)
  let function = def.definition
  let typ = map_poly(c, function.typ)
  let module = c.current_module
  let name = function.name
  let id = get_id(module, name)
  let parameters = list.map(function.parameters, lower_parameter(c, _))
  let body = lower_body(c, function.body)
  let taken = list.map(parameters, fn(param) { param.name })
  let #(_, _, body) = unshadow(taken, 1, body)

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
  c: Context,
  module_name: String,
  custom_typ: Poly,
  variant: Variant,
) {
  // constructor function
  let fun =
    External(
      typ: variant.typ,
      src: "",
      module: module_name,
      id: "new_" <> variant.id,
      mono: True,
    )
  let acc = Context(..c, externals: dict.insert(c.externals, variant.id, fun))

  // variant check function
  let fun_id = gen_names.get_variant_check_name(variant.id)
  let typ = Poly(custom_typ.vars, FunctionType([custom_typ.typ], bool_type))
  let fun =
    External(typ: typ, src: "", module: module_name, id: fun_id, mono: True)
  Context(..acc, externals: dict.insert(acc.externals, fun.id, fun))
}

fn lower_body(c: t.Context, body: List(t.Statement)) {
  case body {
    [] -> {
      io.println(
        "unimplemented function "
        <> c.current_module
        <> "."
        <> c.current_definition,
      )
      lower_expression(c, t.Todo(t.nil_type, None))
    }
    [statement] ->
      case statement {
        t.Expression(_, exp) -> lower_expression(c, exp)
        t.Assignment(_, kind, pattern, _, value) -> {
          let body = lower_expression(c, value)

          // check assertions
          case kind {
            t.Assert -> {
              let subject = t.LocalVariable(value.typ, "S")
              let value = lower_expression(c, value)
              let body = check_assertions(c, pattern, subject, value, body)
              Let(body.typ, "S", value, body)
            }
            t.Let -> body
          }
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

          let bindings = lower_pattern_bindings(c, pattern, subject)
          let body =
            list.fold(bindings, body, fn(body, binding) {
              let #(name, subject) = binding
              let value = lower_expression(c, subject)
              Let(body.typ, name, value, body)
            })

          // check assertions
          let body = case kind {
            t.Assert -> check_assertions(c, pattern, subject, value, body)
            t.Let -> body
          }

          Let(body.typ, "S", value, body)
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
  let message = "Assertion failed in " <> current_location_string(c) <> "\n"
  let inspect_typ = FunctionType([value.typ], string_type)
  let append_typ = FunctionType([string_type, string_type], string_type)
  let m =
    Call(string_type, Global(append_typ, "append_string"), [
      Literal(string_type, String(message)),
      Call(string_type, Global(inspect_typ, "inspect"), [Local(value.typ, "S")]),
    ])
  let els = Op(body.typ, Panic, [m])
  If(body.typ, match, body, els)
}

type BitArrayMode {
  BitsMode
  BytesMode
  IntMode
  Utf8Mode
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
        t.BitsOption -> Ok(BitsMode)
        t.BytesOption -> Ok(BytesMode)
        t.FloatOption -> todo
        t.IntOption -> Ok(IntMode)
        t.Utf16CodepointOption -> todo
        t.Utf16Option -> todo
        t.Utf32CodepointOption -> todo
        t.Utf32Option -> todo
        t.Utf8CodepointOption -> todo
        t.Utf8Option -> Ok(Utf8Mode)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case value {
      t.Int(typ, value) -> IntMode
      t.Float(typ, value) -> todo
      t.String(typ, value) -> Utf8Mode
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
      BitsMode -> Unsized
      BytesMode -> Unsized
      Utf8Mode -> Unsized
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
        t.BitsOption -> Ok(BitsMode)
        t.BytesOption -> Ok(BytesMode)
        t.FloatOption -> todo
        t.IntOption -> Ok(IntMode)
        t.Utf16CodepointOption -> todo
        t.Utf16Option -> todo
        t.Utf32CodepointOption -> todo
        t.Utf32Option -> todo
        t.Utf8CodepointOption -> todo
        t.Utf8Option -> Ok(Utf8Mode)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap(case value {
      t.PatternInt(typ, value) -> IntMode
      t.PatternFloat(typ, value) -> todo
      t.PatternString(typ, value) -> Utf8Mode
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
      BitsMode -> Unsized
      BytesMode -> Unsized
      Utf8Mode -> Unsized
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
        Sized(size) -> #(size, False)
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

      let inner_subject =
        t.Call(
          t.int_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.int_type,
            ),
            t.builtin,
            "index_bit_array_int",
            [],
          ),
          [subject, offset, size],
        )

      #(size, False, inner_subject)
    }

    Utf8Mode -> {
      let size = case size_value {
        Sized(_size) -> panic as "size not supported"
        Unsized ->
          case expr {
            t.PatternString(_typ, s) -> {
              let assert Ok(unescaped) = glexer.unescape_string(s)
              let size = string.byte_size(unescaped)
              t.Int(t.int_type, int.to_string(8 * size))
            }
            _ -> {
              // TODO actually need to read a utf8 char (variable size)?
              t.Int(t.int_type, "8")
            }
          }
      }

      let inner_subject =
        t.Call(
          t.string_type,
          t.Function(
            t.FunctionType(
              [t.bit_array_type, t.int_type, t.int_type],
              t.string_type,
            ),
            t.builtin,
            "index_bit_array_string",
            [],
          ),
          [subject, offset, size],
        )

      #(size, False, inner_subject)
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
    t.PatternConcatenate(typ, prefix, prefix_name, suffix_name) -> {
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
    t.PatternBitString(typ, segs) -> {
      // TODO total size not used? can we remove the calculation?
      let #(total_size, segs) =
        list.fold(segs, #(t.Int(t.int_type, "0"), []), fn(acc, seg) {
          let #(offset, bindings) = acc
          let #(pattern, options) = seg

          let #(size, _match_to_end, inner_subject) =
            index_bit_array(options, subject, offset, pattern)

          let offset = t.BinaryOperator(t.int_type, g.AddInt, offset, size)
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

pub const nil_type = NamedType("Nil", [])

pub const bool_type = NamedType("Bool", [])

pub const int_type = NamedType("Int", [])

pub const string_type = NamedType("String", [])

pub const true_value = Literal(bool_type, Bool("True"))

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
    t.PatternAssignment(typ, pattern, name) -> {
      lower_pattern_match(c, pattern, subject)
    }
    t.PatternConcatenate(typ, prefix, prefix_name, suffix_name) -> {
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
    t.PatternBitString(typ, segs) -> {
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

fn lower_expression(c: t.Context, exp: t.Expression) -> Exp {
  case exp {
    t.Int(typ, value) -> Literal(map_type(c, typ), Int(value))
    t.Float(typ, value) -> Literal(map_type(c, typ), Float(value))
    t.String(typ, value) -> Literal(map_type(c, typ), String(value))
    t.LocalVariable(typ, name) -> Local(map_type(c, typ), name)
    t.Function(typ, module, name, labels) -> {
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
    t.Constant(value:, ..) -> {
      // inline the constant
      lower_expression(c, value)
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
        None -> {
          let message = "panic: " <> current_location_string(c)
          Literal(map_type(c, t.string_type), String(message))
        }
      }
      Op(typ, Panic, [value])
    }
    t.Todo(typ, value) -> {
      let typ = map_type(c, typ)
      let value = case value {
        Some(value) -> lower_expression(c, value)
        None -> {
          let message = "todo: " <> current_location_string(c)
          Literal(map_type(c, t.string_type), String(message))
        }
      }
      Op(typ, Panic, [value])
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
      let subject_name = case record {
        t.LocalVariable(_, name) -> name
        _ -> "S"
      }
      let subject = Local(typ, subject_name)
      let fields =
        list.index_map(ordered_fields, fn(f, i) {
          case f {
            Ok(field) -> lower_expression(c, field)
            Error(field_type) -> {
              let field_type = map_type(c, field_type)
              Op(field_type, FieldAccess(constructor, i), [subject])
            }
          }
        })
      let record = lower_expression(c, record)
      let field_types = list.map(fields, fn(x) { x.typ })
      let constructor_typ = FunctionType(field_types, typ)
      let constructor = gen_names.get_id(module, constructor)
      let body = Call(typ, Global(constructor_typ, constructor), fields)
      Let(body.typ, subject_name, record, body)
    }
    t.FieldAccess(typ, container, module, variant, label, i) -> {
      let typ = map_type(c, typ)
      let container = lower_expression(c, container)
      Op(typ, FieldAccess(variant, i), [container])
    }
    t.Call(typ, function, args) -> {
      let typ = map_type(c, typ)
      let function = lower_expression(c, function)
      let arguments = list.map(args, lower_expression(c, _))
      Call(typ, function, arguments)
    }
    t.TupleIndex(typ, tuple, index) -> {
      let typ = map_type(c, typ)
      let tuple = lower_expression(c, tuple)
      Op(typ, FieldAccess("#", index), [tuple])
    }
    t.FnCapture(typ, label, function, arguments_before, arguments_after) -> todo
    t.BitString(typ, segs) -> {
      let typ = map_type(c, typ)

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
                Utf8Mode ->
                  case exp {
                    Literal(_, String(s)) -> {
                      let assert Ok(unescaped) = glexer.unescape_string(s)
                      let size = string.byte_size(unescaped)
                      Literal(int_type, Int(int.to_string(8 * size)))
                    }
                    _ -> panic as "expected string value for utf8"
                  }
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
            BitsMode | BytesMode -> "write_bit_array"
            Utf8Mode -> "write_bit_array_string"
          }

          let write_typ =
            FunctionType([seg_value.typ, typ, int_type, int_type], nil_type)
          let write_fun = Global(write_typ, write_fun_name)
          let write_call =
            Call(nil_type, write_fun, [seg_value, target, offset, seg_size])

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
        Op(typ, Panic, [
          Literal(
            map_type(c, t.string_type),
            String("No matching clause in " <> current_location_string(c)),
          ),
        ])

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
    t.BinaryOperator(typ, g.NotEq, left, right) -> {
      let not_eq = t.NegateBool(typ, t.BinaryOperator(typ, g.Eq, left, right))
      lower_expression(c, not_eq)
    }
    t.BinaryOperator(typ, name, left, right) -> {
      let typ = map_type(c, typ)
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
  }
}

fn current_location_string(c: t.Context) {
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
  }
}

fn map_poly(c: t.Context, typ: t.Poly) {
  Poly(typ.vars, map_type(c, typ.typ))
}

fn register_tuple(c: Context, size: Int) {
  let id = gen_names.get_tuple_id(size)
  let vars = listx.sane_range(size)
  let element_types = list.map(vars, fn(i) { Unbound(i) })
  let custom_typ = Poly(vars, NamedType(id, element_types))
  let constructor_typ = Poly(vars, FunctionType(element_types, custom_typ.typ))

  let element_fields =
    list.index_map(element_types, fn(typ, i) {
      Parameter(typ, gen_names.get_field_name("", i))
    })

  let variant = Variant(constructor_typ, id, "#", element_fields)
  let custom = CustomType(custom_typ, id, "#", [variant])

  let c = Context(..c, types: dict.insert(c.types, custom.id, custom))
  register_variant_functions(c, t.builtin, custom.typ, variant)
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
      let parameters = list.map(elements, map_type(c, _))
      NamedType(gen_names.get_tuple_id(list.length(elements)), parameters)
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
      let #(_, i, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var.name) {
            True -> {
              let new_var = var.name <> "V" <> int.to_string(i)
              let i = i + 1
              let exp = replace_var(var.name, Local(var.typ, new_var), exp)
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
  }
}

/// Convert a Type to a string representation
pub fn type_to_string(typ: Type) -> String {
  case typ {
    NamedType(id, []) -> id
    NamedType(id, parameters) ->
      id
      <> "("
      <> string.join(list.map(parameters, type_to_string), ", ")
      <> ")"
    FunctionType([], return) -> "fn() -> " <> type_to_string(return)
    FunctionType(parameters, return) ->
      "fn("
      <> string.join(list.map(parameters, type_to_string), ", ")
      <> ") -> "
      <> type_to_string(return)
    Unbound(id) -> "?" <> int.to_string(id)
  }
}

/// Convert a LiteralKind to a string representation
pub fn literal_to_string(literal: LiteralKind) -> String {
  case literal {
    NilVal -> "Nil"
    Bool(value) -> value
    Int(value) -> value
    Float(value) -> value
    String(value) -> "\"" <> value <> "\""
    BitArray(size) -> "<<" <> size <> ">>"
  }
}

/// Convert a Poly to a string representation
pub fn poly_to_string(poly: Poly) -> String {
  case poly {
    Poly([], typ) -> type_to_string(typ)
    Poly(vars, typ) -> {
      let var_strings = list.map(vars, fn(var) { "?" <> int.to_string(var) })
      "forall " <> string.join(var_strings, " ") <> ". " <> type_to_string(typ)
    }
  }
}

/// Convert a Parameter to a string representation
pub fn parameter_to_string(param: Parameter) -> String {
  param.name <> ": " <> type_to_string(param.typ)
}

/// Convert a Function to a string representation
pub fn function_to_string(func: Function) -> String {
  let Function(typ, id, parameters, body) = func
  let param_strings = list.map(parameters, parameter_to_string)
  "fn "
  <> id
  <> "("
  <> string.join(param_strings, ", ")
  <> ") : "
  <> poly_to_string(typ)
  <> " {\n"
  <> "  "
  <> exp_to_string_with_indent(body, 1)
  <> "\n"
  <> "}"
}

/// Convert an Exp to a string representation that looks like source code
/// and includes type information
pub fn exp_to_string(exp: Exp) -> String {
  exp_to_string_with_indent(exp, 0)
}

/// Helper function for exp_to_string with indentation support
fn exp_to_string_with_indent(exp: Exp, indent: Int) -> String {
  let indent_str = string.repeat("  ", indent)
  let next_indent = indent + 1
  let next_indent_str = string.repeat("  ", next_indent)

  case exp {
    Literal(typ, value) ->
      literal_to_string(value) <> " : " <> type_to_string(typ)

    Local(typ, name) -> name <> " : " <> type_to_string(typ)

    Global(typ, id) -> id <> " : " <> type_to_string(typ)

    Fn(typ, parameters, body) -> {
      let param_strings =
        list.map(parameters, fn(param) {
          param.name <> ": " <> type_to_string(param.typ)
        })
      "fn("
      <> string.join(param_strings, ", ")
      <> ") : "
      <> type_to_string(typ)
      <> " {\n"
      <> next_indent_str
      <> exp_to_string_with_indent(body, next_indent)
      <> "\n"
      <> indent_str
      <> "}"
    }

    Call(typ, function, arguments) -> {
      let arg_strings =
        list.map(arguments, fn(arg) { exp_to_string_with_indent(arg, indent) })
      exp_to_string_with_indent(function, indent)
      <> "("
      <> string.join(arg_strings, ", ")
      <> ") : "
      <> type_to_string(typ)
    }

    Op(typ, op, arguments) -> {
      let arg_strings =
        list.map(arguments, fn(arg) { exp_to_string_with_indent(arg, indent) })
      case op {
        FieldAccess(_, field) -> "$get_" <> int.to_string(field)
        Panic -> "$panic"
      }
      <> "("
      <> string.join(arg_strings, ", ")
      <> ") : "
      <> type_to_string(typ)
    }

    Let(typ, name, value, body) ->
      "let "
      <> name
      <> " = "
      <> exp_to_string_with_indent(value, indent)
      <> " in\n"
      <> indent_str
      <> exp_to_string_with_indent(body, indent)
      <> " : "
      <> type_to_string(typ)

    If(typ, condition, then_exp, else_exp) ->
      "if "
      <> exp_to_string_with_indent(condition, indent)
      <> " then\n"
      <> next_indent_str
      <> exp_to_string_with_indent(then_exp, next_indent)
      <> "\n"
      <> indent_str
      <> "else\n"
      <> next_indent_str
      <> exp_to_string_with_indent(else_exp, next_indent)
      <> "\n"
      <> indent_str
      <> ": "
      <> type_to_string(typ)
  }
}
