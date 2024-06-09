import env
import typed as t

import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string

import pprint as pp

type VarName =
  String

type Context {
  Context(poly: t.Context, mono: Module, done: List(String))
}

pub type Module {
  Module(types: List(CustomType), functions: List(Function))
}

pub type Constructor {
  Constructor(name: VarName, variant: Variant)
}

pub type Function {
  Function(name: VarName, params: List(String), body: Exp, typ: Mono)
}

pub type CustomType {
  CustomType(
    name: String,
    params: List(String),
    variants: List(Variant),
    pointer: Bool,
  )
}

pub type Variant {
  Variant(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Mono)
}

pub type Exp {
  Literal(typ: Mono, value: t.LiteralKind)
  Var(typ: Mono, var: VarName)
  Call(typ: Mono, fun: Exp, args: List(Exp))
  Fn(typ: Mono, var: List(VarName), exp: Exp)
  Let(typ: Mono, var: VarName, val: Exp, exp: Exp)
  If(typ: Mono, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type Mono {
  MonoApp(typ: String, args: List(Mono))
  MonoFun(ret: Mono, args: List(Mono))
}

pub fn run(poly: t.Context) {
  let mod = Module([], [])
  let c = Context(poly, mod, [])

  // ensure Nil is instantiated, since we replace unbound types with Nil
  let assert Ok(nil) =
    list.find(poly.types, fn(x) { x.name == #(t.builtin, "Nil") })
  let c = instantiate_custom_type(c, [], nil)

  let name = #(c.poly.name, "main")
  let assert Ok(main) = list.find(poly.functions, fn(x) { x.name == name })
  let #(c, _) = instantiate_function(c, name, sub_type(c, [], main.typ.typ))

  c.mono
}

fn sub_type(c: Context, sub: List(#(Int, Mono)), typ: t.Type) -> Mono {
  case typ {
    t.TypeVar(ref) ->
      case t.get_type_var(c.poly, ref) {
        t.Bound(x) -> sub_type(c, sub, x)
        t.Unbound(x, _level) ->
          case list.find(sub, fn(sub) { sub.0 == x }) {
            Ok(sub) -> sub.1
            Error(_) -> MonoApp(global_name(#(t.builtin, "Nil")), [])
          }
      }
    t.TypeApp(name, args) ->
      MonoApp(global_name(name), list.map(args, sub_type(c, sub, _)))
    t.TypeFun(ret, args) ->
      MonoFun(sub_type(c, sub, ret), list.map(args, sub_type(c, sub, _)))
  }
}

pub fn type_name(mono: Mono) -> String {
  case mono {
    MonoApp(name, []) -> "_" <> name
    MonoApp(name, args) ->
      "_"
      <> name
      <> args
      |> list.map(type_name)
      |> string.concat()
    MonoFun(ret, []) -> "_Closure"
    // "_fn" <> type_name(ret)
    MonoFun(ret, args) -> "_Closure"
    // "_fn"
    // <> args
    // |> list.map(type_name)
    // |> string.concat()
    // <> "_"
    // <> type_name(ret)
  }
}

// instantiation algo
// start from main (assert it is monomorphic / fill in gaps with Nil)
// walk the body until you find a (top level) function call
// while walking convert everything to monomorphic types
// instantiate that function based on the monomorphic local type
// - match the function type with the monomorphic type
// - use the match to replace polymorphic type variables

fn get_type_string(sub: List(#(Int, Mono))) {
  list.map(sub, fn(s) { type_name(s.1) })
  |> string.concat()
}

fn instantiate_function(
  c: Context,
  name: #(String, String),
  typ: Mono,
) -> #(Context, String) {
  let assert Ok(fun) = list.find(c.poly.functions, fn(x) { x.name == name })

  let sub = unify_poly(c, fun.typ, typ)

  let type_string = get_type_string(sub)

  // unify types to find a substitution

  // get the name of the instantiated function
  let mono_name = "F_" <> global_name(fun.name) <> type_string

  // check if already instantiated
  case list.contains(c.done, mono_name) {
    True -> #(c, mono_name)
    False -> {
      // mark as done
      let c = Context(..c, done: [mono_name, ..c.done])

      // instantiate function
      let #(c, mono_body) = typed_to_mono_exp(c, sub, fun.body)
      let params = list.map(fun.params, fn(x) { x.name })
      let fun = Function(mono_name, params, mono_body, typ)

      // add to module
      let funs = [fun, ..c.mono.functions]
      let c = Context(..c, mono: Module(..c.mono, functions: funs))

      #(c, mono_name)
    }
  }
}

fn instantiate_custom_type(
  c: Context,
  sub: List(#(Int, Mono)),
  custom: t.CustomType,
) {
  let type_string = get_type_string(sub)
  let custom_mono_name = global_name(custom.name) <> type_string

  // check if custom type is already instantiated
  case list.contains(c.done, custom_mono_name) {
    True -> c
    False -> {
      // mark as done
      let c = Context(..c, done: [custom_mono_name, ..c.done])

      // instantiate type
      let variants =
        list.map(custom.variants, fn(v) {
          let variant_mono_name = global_name(v.name) <> type_string
          let fields =
            list.map(v.fields, fn(f) { Field(f.name, sub_type(c, sub, f.typ)) })
          Variant(variant_mono_name, fields)
        })
      let pointer = case variants {
        [_] -> False
        _ -> True
      }
      let custom =
        CustomType(custom_mono_name, custom.params, variants, pointer)

      // add to module
      let types = [custom, ..c.mono.types]
      let c = Context(..c, mono: Module(..c.mono, types: types))

      c
    }
  }
}

pub fn global_name(name: #(String, String)) -> String {
  case name {
    #(mod, name) if mod == t.builtin -> name
    #(mod, name) -> string.replace(mod, "/", "_") <> "_" <> name
  }
}

fn typed_to_mono_exp(
  c: Context,
  sub: List(#(Int, Mono)),
  e: t.Exp,
) -> #(Context, Exp) {
  case e {
    t.Literal(typ, v) -> #(c, Literal(sub_type(c, sub, typ), v))
    t.Var(typ, name) -> {
      let typ = sub_type(c, sub, typ)
      #(c, Var(typ, name))
    }
    t.GlobalVar(typ, name) -> {
      let assert Ok(kind) = env.get(c.poly.global_env, name)
      let typ = sub_type(c, sub, typ)
      case kind {
        t.BuiltInVar(_) -> {
          #(c, Var(typ, name.1))
        }
        t.BuiltInPolyVar(poly) -> {
          let sub = unify_poly(c, poly, typ)
          let type_string = get_type_string(sub)
          let mono_name = global_name(name) <> type_string
          #(c, Var(typ, mono_name))
        }
        t.FunctionVar(_) -> {
          let #(c, mono_name) = instantiate_function(c, name, typ)
          #(c, Var(typ, mono_name))
        }
        t.ConstructorVar(poly, variant, custom) -> {
          let sub = unify_poly(c, poly, typ)

          let c = instantiate_custom_type(c, sub, custom)

          let type_string = get_type_string(sub)
          let mono_name = global_name(variant.name) <> type_string <> "_NEW"

          #(c, Var(typ, mono_name))
        }
        t.IsaVar(poly, variant, custom) -> {
          let sub = unify_poly(c, poly, typ)

          let c = instantiate_custom_type(c, sub, custom)

          let type_string = get_type_string(sub)
          let mono_name = global_name(variant.name) <> type_string <> "_IS"

          #(c, Var(typ, mono_name))
        }
        t.GetterVar(poly, field, variant, custom) -> {
          let sub = unify_poly(c, poly, typ)

          let c = instantiate_custom_type(c, sub, custom)

          let type_string = get_type_string(sub)
          let mono_name =
            global_name(variant.name) <> type_string <> "_" <> field.name

          #(c, Var(typ, mono_name))
        }
      }
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
      #(c, Call(typ, fun, args))
    }
    t.Fn(typ, vars, exp) -> {
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(c, Fn(typ, vars, exp))
    }
    t.Let(typ, var, val, exp) -> {
      let #(c, val) = typed_to_mono_exp(c, sub, val)
      let #(c, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(c, Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = typed_to_mono_exp(c, sub, cond)
      let #(c, then_e) = typed_to_mono_exp(c, sub, then_e)
      let #(c, else_e) = typed_to_mono_exp(c, sub, else_e)
      let typ = sub_type(c, sub, typ)
      #(c, If(typ, cond, then_e, else_e))
    }
  }
}

fn unify_poly(c: Context, poly: t.Poly, mono: Mono) -> List(#(Int, Mono)) {
  let sub = unify_type(c, poly.typ, mono)
  list.map(poly.vars, fn(x) {
    case list.find(sub, fn(s) { s.0 == x }) {
      Ok(s) -> #(x, s.1)
      Error(Nil) -> {
        io.debug(poly)
        io.debug(mono)
        io.debug(sub)
        panic as "could not unify poly type"
      }
    }
  })
}

fn unify_type(c: Context, poly: t.Type, mono: Mono) -> List(#(Int, Mono)) {
  case poly, mono {
    t.TypeVar(ref), _ ->
      case t.get_type_var(c.poly, ref) {
        t.Bound(x) -> unify_type(c, x, mono)
        t.Unbound(x, _) -> [#(x, mono)]
      }
    t.TypeApp(a, aa), MonoApp(b, ba) ->
      case global_name(a) {
        a if a == b ->
          list.zip(aa, ba)
          |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) })
        _ -> {
          io.debug(poly)
          io.debug(mono)
          panic as "could not unify types"
        }
      }
    t.TypeFun(ar, aa), MonoFun(br, ba) ->
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
