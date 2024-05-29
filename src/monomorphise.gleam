import typed as t

import gleam/io
import gleam/list
import gleam/string

type VarName =
  String

type Context {
  Context(poly: t.Context, mono: Module, done: List(String))
}

pub type Module {
  Module(types: List(CustomType), functions: List(Function))
}

pub type Function {
  Function(name: VarName, params: List(String), body: Exp, typ: Mono)
}

pub type CustomType {
  TypeDef(name: String, params: List(String), variants: List(Variant))
}

pub type Variant {
  VariantDef(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Mono)
}

pub type Exp {
  Int(typ: Mono, val: String)
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

  let assert Ok(main) = list.find(poly.functions, fn(x) { x.name == "main" })
  let #(c, _) = instantiate(c, main, sub_type(c, [], main.typ.typ))

  // list.each(c.mono.functions, fn(f) { io.debug(#(f.name, f.typ)) })

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
            // TODO id(id) causes the problem because theres nothing
            // binding what type of id the returned function uses
            // maybe we just fill in with Nil instead of panic
            Error(_) -> panic as "unbound type variable"
          }
      }
    t.TypeApp(name, args) -> MonoApp(name, list.map(args, sub_type(c, sub, _)))
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
    MonoFun(ret, []) -> "_fn" <> type_name(ret)
    MonoFun(ret, args) ->
      "_fn"
      <> args
      |> list.map(type_name)
      |> string.concat()
      <> "_"
      <> type_name(ret)
  }
}

fn mono_function_name(fun: t.Function, sub: List(#(Int, Mono))) -> String {
  "G_"
  <> fun.name
  <> list.map(sub, fn(s) { type_name(s.1) })
  |> string.concat()
}

// instantiation algo
// start from main (assert it is monomorphic / fill in gaps with Nil)
// walk the body until you find a (top level) function call
// while walking convert everything to monomorphic types
// instantiate that function based on the monomorphic local type
// - match the function type with the monomorphic type
// - use the match to replace polymorphic type variables

fn instantiate(c: Context, fun: t.Function, typ: Mono) -> #(Context, String) {
  // unify types to find a substitution
  let sub = unify_poly(c, fun.typ, typ)

  // get the name of the instantiated function
  let mono_name = mono_function_name(fun, sub)

  // check if already instantiated
  case list.contains(c.done, mono_name) {
    True -> #(c, mono_name)
    False -> {
      // mark as done before doing recursive call
      let c = Context(..c, done: [mono_name, ..c.done])
      let #(c, mono_body) = typed_to_mono_exp(c, sub, fun.body)

      // add the monomorphised function
      let fun = Function(mono_name, fun.params, mono_body, typ)
      let functions = [fun, ..c.mono.functions]
      let c = Context(..c, mono: Module(..c.mono, functions: functions))

      #(c, mono_name)
    }
  }
}

fn typed_to_mono_exp(
  c: Context,
  sub: List(#(Int, Mono)),
  e: t.Exp,
) -> #(Context, Exp) {
  case e {
    t.Int(typ, v) -> #(c, Int(sub_type(c, sub, typ), v))
    t.Var(typ, var) -> {
      // TODO need to unshadow first or keep track of a local env
      case list.find(c.poly.functions, fn(x) { x.name == var }) {
        Ok(fun) -> {
          let typ = sub_type(c, sub, typ)
          let #(c, mono_name) = instantiate(c, fun, typ)
          #(c, Var(typ, mono_name))
        }
        Error(_) -> {
          let typ = sub_type(c, sub, typ)
          #(c, Var(typ, var))
        }
      }
    }
    t.Call(typ, fun, args) -> {
      let #(m, fun) = typed_to_mono_exp(c, sub, fun)
      let #(m, args) =
        list.fold(args, #(m, []), fn(acc, arg) {
          let #(m, args) = acc
          let #(m, arg) = typed_to_mono_exp(m, sub, arg)
          #(m, [arg, ..args])
        })
      let args = list.reverse(args)
      let typ = sub_type(c, sub, typ)
      #(m, Call(typ, fun, args))
    }
    t.Fn(typ, vars, exp) -> {
      let #(m, exp) = typed_to_mono_exp(c, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(m, Fn(typ, vars, exp))
    }
    t.Let(typ, var, val, exp) -> {
      let #(m, val) = typed_to_mono_exp(c, sub, val)
      let #(m, exp) = typed_to_mono_exp(m, sub, exp)
      let typ = sub_type(c, sub, typ)
      #(m, Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(m, cond) = typed_to_mono_exp(c, sub, cond)
      let #(m, then_e) = typed_to_mono_exp(m, sub, then_e)
      let #(m, else_e) = typed_to_mono_exp(m, sub, else_e)
      let typ = sub_type(c, sub, typ)
      #(m, If(typ, cond, then_e, else_e))
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
    t.TypeApp(a, aa), MonoApp(b, ba) if a == b ->
      list.zip(aa, ba)
      |> list.flat_map(fn(x) { unify_type(c, x.0, x.1) })
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
