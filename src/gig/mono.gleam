import gig/core as t
import gleam/dict
import gleam/io
import gleam/list
import gleam/set
import gleam/string

pub type Context {
  Context(
    in_types: dict.Dict(String, t.CustomType),
    in_functions: dict.Dict(String, t.Function),
    in_externals: dict.Dict(String, t.External),
    types: List(CustomType),
    functions: List(Function),
    externals: List(External),
    used_types: set.Set(String),
    used_functions: set.Set(String),
    used_externals: set.Set(String),
    used_modules: set.Set(String),
  )
}

pub type Type {
  NamedType(id: String, parameters: List(Type))
  FunctionType(parameters: List(Type), return: Type)
}

pub type External {
  External(
    typ: Type,
    module: String,
    internal_name: String,
    external_name: String,
    parameters: List(Field),
    mono: Bool,
  )
}

pub type CustomType {
  CustomType(
    name: String,
    untyped_name: String,
    display_name: String,
    variants: List(Variant),
  )
}

pub type Variant {
  Variant(
    name: String,
    untyped_name: String,
    display_name: String,
    fields: List(Field),
  )
}

pub type Field {
  Field(name: String, typ: Type)
}

pub type Function {
  Function(name: String, parameters: List(Field), return: Type, body: Exp)
}

pub type FieldAccessKind {
  StructAccess
  StructPointerAccess
  TaggedUnionAccess
}

pub type Op {
  FieldAccess(kind: FieldAccessKind, variant: String, field: String)
  VariantCheck(variant: String)
}

pub type Exp {
  Literal(typ: Type, val: t.LiteralKind)
  Local(typ: Type, var: String)
  Global(typ: Type, var: String)
  Call(typ: Type, fun: Exp, arg: List(Exp))
  Op(typ: Type, op: Op, arg: List(Exp))
  // CallClosure(typ: Type, fun: Exp, arg: List(Exp))
  Fn(typ: Type, parameters: List(Field), body: Exp)
  Let(typ: Type, var: String, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
  Panic(typ: Type, value: Exp)
}

pub fn init_context(in: t.Context) -> Context {
  let in_types =
    list.fold(in.types, dict.new(), fn(d, i) { dict.insert(d, i.id, i) })
  let in_functions =
    list.fold(in.functions, dict.new(), fn(d, i) { dict.insert(d, i.id, i) })
  let in_externals =
    list.fold(in.externals, dict.new(), fn(d, i) {
      dict.insert(d, i.internal_name, i)
    })
  Context(
    in_types:,
    in_functions:,
    in_externals:,
    types: [],
    functions: [],
    externals: [],
    used_types: set.new(),
    used_functions: set.new(),
    used_externals: set.new(),
    used_modules: set.new(),
  )
}

pub fn run(in: t.Context, main_name: String) {
  let c = init_context(in)

  let main = case dict.get(c.in_functions, main_name) {
    Ok(main) -> main
    Error(_) -> panic as "main function not found"
  }

  // instantiate types and functions reachable from main
  let typ = sub_type([], main.typ.typ)
  let #(c, main_name) = instantiate_function(c, main_name, typ)

  // instantiate types used by externals
  let c =
    in.externals
    |> list.filter(fn(x) { set.contains(c.used_modules, x.module) })
    |> list.filter(fn(x) { !x.mono })
    |> list.fold(c, fn(c, external) {
      let used_externals = set.insert(c.used_externals, external.internal_name)
      let c = Context(..c, used_externals:)
      instantiate_type(c, sub_type([], external.typ.typ))
    })

  #(c, main_name)
}

pub fn sub_type(sub: List(#(Int, Type)), typ: t.Type) -> Type {
  case typ {
    t.Unbound(id) ->
      case list.find(sub, fn(sub) { sub.0 == id }) {
        Ok(sub) -> sub.1
        Error(_) -> NamedType("Nil", [])
      }
    t.NamedType(name, args) -> NamedType(name, list.map(args, sub_type(sub, _)))
    t.FunctionType(args, ret) ->
      FunctionType(list.map(args, sub_type(sub, _)), sub_type(sub, ret))
  }
}

fn unify_poly(c: Context, poly: t.Poly, mono: Type) -> List(#(Int, Type)) {
  let sub = unify_type(c, poly.typ, mono)
  list.map(poly.vars, fn(x) {
    case list.find(sub, fn(s) { s.0 == x }) {
      Ok(s) -> #(x, s.1)
      Error(Nil) -> {
        panic as "could not unify poly type"
      }
    }
  })
}

fn unify_type(c: Context, poly: t.Type, mono: Type) -> List(#(Int, Type)) {
  case poly, mono {
    t.Unbound(id), _ -> [#(id, mono)]
    t.NamedType(a, aa), NamedType(b, ba) ->
      case a == b {
        True ->
          list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) })
        False -> {
          panic as "could not unify types"
        }
      }
    t.FunctionType(aa, ar), FunctionType(ba, br) ->
      list.append(
        unify_type(c, ar, br),
        list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) }),
      )
    _, _ -> {
      panic as "could not unify types"
    }
  }
}

pub fn type_name(typ: Type) -> String {
  case typ {
    NamedType(name, args) ->
      string.join([name, ..list.map(args, type_name)], "_")
    FunctionType(..) -> "Closure"
  }
}

fn get_type_string(sub: List(#(Int, Type))) {
  // TODO only include typed that are actually used
  // e.g. variant that only uses part of the params
  // e.g. phantom type
  list.map(sub, fn(s) { "_" <> type_name(s.1) })
  |> string.concat()
}

pub fn instantiate_type(c: Context, mono: Type) {
  case mono {
    NamedType(name, _args) -> {
      case dict.get(c.in_types, name) {
        Ok(custom) -> {
          let sub = unify_poly(c, custom.typ, mono)
          let type_string = get_type_string(sub)
          let mono_name = custom.id <> type_string

          case set.contains(c.used_types, mono_name) {
            True -> c
            False -> {
              // instantiate variants
              let variants =
                list.map(custom.variants, fn(v) {
                  let mono_name = v.id <> type_string
                  let fields =
                    list.map(v.fields, fn(field) {
                      Field(field.name, sub_type(sub, field.typ))
                    })
                  Variant(mono_name, v.id, v.display_name, fields)
                })

              // create new custom type
              let custom =
                CustomType(mono_name, custom.id, custom.display_name, variants)

              // add to module
              let types = [custom, ..c.types]
              let used_types = set.insert(c.used_types, mono_name)

              let c = Context(..c, types:, used_types:)

              // also instantiate any types referenced by this type
              list.fold(variants, c, fn(c, variant) {
                list.fold(variant.fields, c, fn(c, field) {
                  instantiate_type(c, field.typ)
                })
              })
            }
          }
        }
        Error(_) -> c
      }
    }
    FunctionType(args, ret) -> {
      let c = list.fold(args, c, instantiate_type)
      instantiate_type(c, ret)
    }
  }
}

fn instantiate_function(c: Context, name: String, mono: Type) {
  case dict.get(c.in_functions, name) {
    Ok(fun) -> {
      let sub = unify_poly(c, fun.typ, mono)
      let type_string = get_type_string(sub)
      let mono_name = fun.id <> type_string

      case fun.body {
        t.Panic(_, _) -> {
          io.println_error("instantiating unimplemented function " <> fun.id)
        }
        _ -> Nil
      }

      case set.contains(c.used_functions, mono_name) {
        True -> #(c, mono_name)
        False -> {
          let used_functions = set.insert(c.used_functions, mono_name)
          let c = Context(..c, used_functions:)

          // instantiate function
          let #(c, mono_body) = typed_to_mono_exp(c, sub, fun.body)
          let params =
            list.map(fun.parameters, fn(p) {
              let typ = sub_type(sub, p.typ)
              Field(p.name, typ)
            })
          let assert FunctionType(_, ret) = mono
          let fun =
            Function(
              name: mono_name,
              parameters: params,
              return: ret,
              body: mono_body,
            )

          // add to module
          let functions = [fun, ..c.functions]
          let c = Context(..c, functions:)

          #(c, mono_name)
        }
      }
    }
    Error(_) ->
      case dict.get(c.in_externals, name) {
        Ok(external) ->
          case set.contains(c.used_externals, external.internal_name) {
            True -> #(c, external.external_name)
            False -> {
              let sub = unify_poly(c, external.typ, mono)
              let type_string = get_type_string(sub)
              case external.mono {
                True -> #(c, external.external_name <> type_string)
                False -> {
                  let typ = sub_type([], external.typ.typ)
                  let c = instantiate_type(c, typ)

                  let params =
                    list.map(external.parameters, fn(param) {
                      Field(param.name, sub_type([], param.typ))
                    })

                  // add the external to output
                  let external =
                    External(
                      internal_name: external.internal_name,
                      external_name: external.external_name,
                      module: external.module,
                      parameters: params,
                      mono: external.mono,
                      typ: typ,
                    )
                  let externals = [external, ..c.externals]
                  let used_modules = set.insert(c.used_modules, external.module)
                  let used_externals =
                    set.insert(c.used_externals, external.internal_name)

                  let c =
                    Context(..c, externals:, used_modules:, used_externals:)
                  #(c, external.external_name)
                }
              }
            }
          }
        Error(_) -> {
          panic as { "invalid reference " <> name }
        }
      }
  }
}

fn typed_to_mono_exp(
  c: Context,
  sub: List(#(Int, Type)),
  e: t.Exp,
) -> #(Context, Exp) {
  let c = instantiate_type(c, sub_type(sub, e.typ))
  case e {
    t.Literal(typ, v) -> #(c, Literal(sub_type(sub, typ), v))
    t.Local(typ, name) -> {
      let typ = sub_type(sub, typ)
      #(c, Local(typ, name))
    }
    t.Global(typ, name) -> {
      let typ = sub_type(sub, typ)
      let #(c, name) = instantiate_function(c, name, typ)
      #(c, Global(typ, name))
    }
    t.Call(typ, fun, args) -> {
      let #(c, fun) = typed_to_mono_exp(c, sub, fun)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = typed_to_mono_exp(c, sub, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      let typ = sub_type(sub, typ)
      #(c, Call(typ, fun, args))
    }
    t.Op(typ, op, args) -> {
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = typed_to_mono_exp(c, sub, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      let typ = sub_type(sub, typ)
      let op = case op {
        t.FieldAccess(variant:, field:) -> {
          let assert [arg, ..] = args
          let assert NamedType(name, _) = arg.typ
          let assert Ok(custom) = dict.get(c.in_types, name)
          let kind = case custom.variants {
            [_] -> StructAccess
            _ -> TaggedUnionAccess
          }
          FieldAccess(kind:, variant:, field:)
        }
        t.VariantCheck(variant:) -> VariantCheck(variant:)
      }
      #(c, Op(typ, op, args))
    }
    t.Fn(typ, params, exp) -> {
      let typ = sub_type(sub, typ)
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let params =
        list.map(params, fn(p) {
          let typ = sub_type(sub, p.typ)
          Field(p.name, typ)
        })
      #(c, Fn(typ, params, exp))
    }
    t.Let(typ, var, val, exp) -> {
      let #(c, val) = typed_to_mono_exp(c, sub, val)
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(sub, typ)
      #(c, Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = typed_to_mono_exp(c, sub, cond)
      let #(c, then_e) = typed_to_mono_exp(c, sub, then_e)
      let #(c, else_e) = typed_to_mono_exp(c, sub, else_e)
      let typ = sub_type(sub, typ)
      #(c, If(typ, cond, then_e, else_e))
    }
    t.Panic(typ, arg) -> {
      let #(c, arg) = typed_to_mono_exp(c, sub, arg)
      let typ = sub_type(sub, typ)
      #(c, Panic(typ, arg))
    }
  }
}
