import env
import gig/core as t
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import pprint

pub type Context {
  Context(in: t.Context, out: t.Context)
}

pub fn run(c: t.Context, main_name: String) {
  let oc = t.Context(env.new(), env.new())
  let c = Context(in: c, out: oc)
  pprint.debug(c.in.functions)
  pprint.debug(main_name)
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
    t.TupleType(elems) -> t.TupleType(list.map(elems, sub_type(c, sub, _)))
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
    t.TupleType(args) ->
      "_"
      <> "Tuple"
      <> int.to_string(list.length(args))
      <> args
      |> list.map(type_name)
      |> string.concat()

    t.FunctionType(ret, args) -> "_Closure"
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
                  let typ = t.Poly([], sub_type(c, sub, v.typ.typ))
                  let mono_name = v.id <> type_string
                  let fields =
                    list.map(v.fields, fn(typ) { sub_type(c, sub, typ) })
                  t.Variant(typ, mono_name, fields)
                })

              let parameters = custom.parameters

              // create new custom type
              let custom =
                t.CustomType(t.Poly([], mono), mono_name, parameters, variants)

              // add to module
              let types = env.put(c.out.types, mono_name, custom)
              Context(..c, out: t.Context(..c.out, types:))
            }
          }
        }
        Error(_) -> c
      }
    }
    // TODO tuples
    t.TupleType(args) -> c
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
        Ok(fun) -> #(c, fun.id)
        Error(_) -> {
          case fun.external {
            True ->
              case fun.monomorphise {
                True -> #(c, mono_name)
                False -> #(c, name)
              }
            False -> {
              // add placeholder to prevent infinite recursion
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
                  external: False,
                  monomorphise: False,
                )

              // add to module
              let functions = env.put(c.out.functions, mono_name, fun)
              let c = Context(..c, out: t.Context(..c.out, functions:))

              #(c, mono_name)
            }
          }
        }
      }
    }
    Error(_) -> {
      // probably wasn't important..
      #(c, name)
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
        t.FunctionType(_, ret) -> instantiate_type(c, ret)
        _ -> instantiate_type(c, e.typ)
      }
      let typ = sub_type(c, sub, typ)
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
