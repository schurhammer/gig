import gig/core as t
import gig/env
import gig/gen_names

import gleam/io
import gleam/list
import gleam/string

pub type Context {
  Context(in: t.Context, out: t.Context)
}

pub fn run(c: t.Context, main_name: String) {
  let oc =
    t.Context(types: env.new(), functions: env.new(), externals: env.new())
  let c = Context(in: c, out: oc)

  let assert Ok(main) = env.get(c.in.functions, main_name)
  let typ = sub_type(c, [], main.typ.typ)
  let #(c, main_name) = instantiate_function(c, main_name, typ)
  c
}

fn sub_type(c: Context, sub: List(#(Int, t.Type)), typ: t.Type) -> t.Type {
  case typ {
    t.Unbound(id) ->
      case list.find(sub, fn(sub) { sub.0 == id }) {
        Ok(sub) -> sub.1
        Error(_) -> t.NamedType("Nil", [])
      }
    t.TupleType(elements) -> {
      // rewrite to named type
      let variant_id = gen_names.get_tuple_id(list.length(elements))
      let typ = t.NamedType(variant_id, elements)
      sub_type(c, sub, typ)
    }
    t.NamedType(name, args) ->
      t.NamedType(name, list.map(args, sub_type(c, sub, _)))
    t.FunctionType(args, ret) ->
      t.FunctionType(list.map(args, sub_type(c, sub, _)), sub_type(c, sub, ret))
  }
}

fn unify_poly(c: Context, poly: t.Poly, mono: t.Type) -> List(#(Int, t.Type)) {
  let sub = unify_type(c, poly.typ, mono)
  list.map(poly.vars, fn(x) {
    case list.find(sub, fn(s) { s.0 == x }) {
      Ok(s) -> #(x, s.1)
      Error(Nil) -> {
        io.debug(poly)
        io.debug(mono)
        panic as "could not unify poly type"
      }
    }
  })
}

fn unify_type(c: Context, poly: t.Type, mono: t.Type) -> List(#(Int, t.Type)) {
  case poly, mono {
    t.Unbound(id), _ -> [#(id, mono)]
    t.NamedType(a, aa), t.NamedType(b, ba) ->
      case a == b {
        True ->
          list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) })
        False -> {
          io.debug(poly)
          io.debug(mono)
          panic as "could not unify types"
        }
      }
    t.FunctionType(aa, ar), t.FunctionType(ba, br) ->
      list.append(
        unify_type(c, ar, br),
        list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) }),
      )
    t.TupleType(elements), _ -> {
      let variant_id = gen_names.get_tuple_id(list.length(elements))
      let poly = t.NamedType(variant_id, elements)
      unify_type(c, poly, mono)
    }
    _, _ -> {
      io.debug(poly)
      io.debug(mono)
      panic as "could not unify types"
    }
  }
}

pub fn type_name(mono: t.Type) -> String {
  case mono {
    t.NamedType(name, args) ->
      "_"
      <> name
      <> args
      |> list.map(type_name)
      |> string.concat()
    t.FunctionType(ret, args) -> "_Closure"
    t.TupleType(args) -> panic as "tuple"
    t.Unbound(_) -> panic as "unbound"
  }
}

fn get_type_string(sub: List(#(Int, t.Type))) {
  list.map(sub, fn(s) { type_name(s.1) })
  |> string.concat()
}

fn instantiate_type(c: Context, typ: t.Type) {
  case typ {
    t.NamedType(name, _args) -> {
      case env.get(c.in.types, name) {
        Ok(custom) -> {
          let mono = sub_type(c, [], typ)
          let sub = unify_poly(c, custom.typ, mono)
          let type_string = get_type_string(sub)
          let mono_name = custom.id <> type_string

          case env.get(c.out.types, mono_name) {
            Ok(_) -> c
            Error(_) -> {
              // instantiate variants
              let variants =
                list.map(custom.variants, fn(v) {
                  let mono_name = v.id <> type_string
                  let typ = t.Poly([], sub_type(c, sub, v.typ.typ))
                  let fields =
                    list.map(v.fields, fn(typ) { sub_type(c, sub, typ) })
                  t.Variant(typ, mono_name, fields)
                })

              // create new custom type
              let custom = t.CustomType(t.Poly([], mono), mono_name, variants)

              // add to module
              let types = env.put(c.out.types, mono_name, custom)
              Context(..c, out: t.Context(..c.out, types:))
            }
          }
        }
        Error(_) -> c
      }
    }
    _ -> c
  }
}

fn register_tuple(c: Context, typ: t.Type) {
  case typ {
    t.TupleType(elements) -> {
      let cin = c.in

      let variant_id = gen_names.get_tuple_id(list.length(elements))
      let vars = list.index_map(elements, fn(_, i) { i })
      let element_types = list.index_map(elements, fn(_, i) { t.Unbound(i) })
      let custom_typ = t.Poly(vars, t.NamedType(variant_id, element_types))
      let variant_typ =
        t.Poly(vars, t.FunctionType(element_types, custom_typ.typ))

      let variant = t.Variant(variant_typ, variant_id, element_types)
      let custom = t.CustomType(custom_typ, variant_id, [variant])
      let cin = t.Context(..cin, types: env.put(cin.types, custom.id, custom))

      // TODO can we reuse the function from core to create constructor/getters?

      // create generic tuple constructor
      let fun_id = gen_names.get_constructor_name(variant_id)
      let fun_typ = t.Poly(vars, t.FunctionType(element_types, custom_typ.typ))
      let fun = t.External(typ: fun_typ, id: fun_id, mono: True)
      let cin = t.Context(..cin, externals: env.put(cin.externals, fun.id, fun))

      // create generic tuple getter functions
      let cin =
        list.index_fold(element_types, cin, fn(cin, field_type, i) {
          let fun_id = gen_names.get_getter_name(variant_id, i)
          let fun_typ =
            t.Poly(vars, t.FunctionType([custom_typ.typ], field_type))
          let fun = t.External(typ: fun_typ, id: fun_id, mono: True)
          t.Context(..cin, externals: env.put(cin.externals, fun.id, fun))
        })

      // create instantiated custom type via rewriting to named type
      // let c = Context(..c, in: cin)
      // let typ = t.NamedType(variant_id, elements)
      // instantiate_type(c, typ)
      Context(..c, in: cin)
    }
    _ -> c
  }
}

fn instantiate_function(c: Context, name: String, mono: t.Type) {
  case env.get(c.in.functions, name) {
    Ok(fun) -> {
      let sub = unify_poly(c, fun.typ, mono)
      let type_string = get_type_string(sub)
      let mono_name = fun.id <> type_string

      case env.get(c.out.functions, mono_name) {
        Ok(_) -> #(c, mono_name)
        Error(_) -> {
          // add placeholder to prevent duplicate monomorphisation
          let functions = env.put(c.out.functions, mono_name, fun)
          let c = Context(..c, out: t.Context(..c.out, functions:))

          // instantiate function
          let #(c, mono_body) = typed_to_mono_exp(c, sub, fun.body)
          let params =
            list.map(fun.parameters, fn(p) {
              let typ = sub_type(c, sub, p.typ)
              t.Parameter(typ, p.name)
            })
          let fun =
            t.Function(
              typ: t.Poly([], mono),
              id: mono_name,
              parameters: params,
              body: mono_body,
            )

          // add to module
          let functions = env.put(c.out.functions, mono_name, fun)
          let c = Context(..c, out: t.Context(..c.out, functions:))

          #(c, mono_name)
        }
      }
    }
    Error(_) ->
      case env.get(c.in.externals, name) {
        Ok(external) -> {
          let sub = unify_poly(c, external.typ, mono)
          let type_string = get_type_string(sub)
          let mono_name = external.id <> type_string
          case external.mono {
            True -> #(c, mono_name)
            False -> #(c, external.id)
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
  sub: List(#(Int, t.Type)),
  e: t.Exp,
) -> #(Context, t.Exp) {
  case e {
    t.Literal(typ, v) -> #(c, t.Literal(sub_type(c, sub, typ), v))
    t.Local(typ, name) -> {
      let typ = sub_type(c, sub, typ)
      #(c, t.Local(typ, name))
    }
    t.Global(typ, name) -> {
      let c = case typ {
        t.FunctionType(_, ret) -> register_tuple(c, ret)
        _ -> register_tuple(c, typ)
      }
      let typ = sub_type(c, sub, typ)
      let c = case typ {
        t.FunctionType(_, ret) -> instantiate_type(c, ret)
        _ -> instantiate_type(c, typ)
      }
      let #(c, name) = instantiate_function(c, name, typ)
      #(c, t.Global(typ, name))
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
      let typ = sub_type(c, sub, typ)
      #(c, t.Call(typ, fun, args))
    }
    t.Fn(typ, vars, exp) -> {
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(c, t.Fn(typ, vars, exp))
    }
    t.Let(typ, var, val, exp) -> {
      let #(c, val) = typed_to_mono_exp(c, sub, val)
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(c, t.Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = typed_to_mono_exp(c, sub, cond)
      let #(c, then_e) = typed_to_mono_exp(c, sub, then_e)
      let #(c, else_e) = typed_to_mono_exp(c, sub, else_e)
      let typ = sub_type(c, sub, typ)
      #(c, t.If(typ, cond, then_e, else_e))
    }
    t.Panic(typ, reason) -> {
      let #(c, reason) = typed_to_mono_exp(c, sub, reason)
      #(c, t.Panic(typ, reason))
    }
  }
}
