import env.{type Env}
import glance as g

import gleam/io
import gleam/list

pub type VarName =
  String

pub type TypeVarRef {
  TypeVarRef(Int)
}

// pub type TypeVarRef =
//   Int

pub type Module {
  Module(types: List(CustomType), functions: List(Function))
}

pub type Function {
  Function(name: VarName, params: List(String), body: Exp, typ: Poly)
}

pub type Type {
  TypeVar(var: TypeVarRef)
  TypeApp(typ: String, args: List(Type))
  TypeFun(ret: Type, args: List(Type))
}

pub type CustomType {
  TypeDef(name: String, params: List(String), variants: List(Variant))
}

pub type Variant {
  VariantDef(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Type)
}

pub type Exp {
  Int(typ: Type, val: String)
  Var(typ: Type, var: VarName)
  Call(typ: Type, fun: Exp, args: List(Exp))
  Fn(typ: Type, var: List(VarName), exp: Exp)
  Let(typ: Type, var: VarName, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type Poly {
  Mono(typ: Type)
  Poly(var: TypeVarRef, typ: Poly)
}

pub type TypeVar {
  Bound(Type)
  // TODO levels?
  Unbound(TypeVarRef, level: Int)
}

pub type TypeEnv =
  Env(VarName, CustomType)

pub type ValueEnv =
  Env(VarName, Poly)

pub type TypeVarEnv =
  Env(TypeVarRef, TypeVar)

pub type Context {
  Context(
    type_env: TypeEnv,
    type_vars: TypeVarEnv,
    functions: List(Function),
    uid: Int,
  )
}

pub fn infer_module(mod: g.Module) {
  let c =
    Context(type_env: env.new(), type_vars: env.new(), functions: [], uid: 0)

  // TODO create env correctly
  let #(c, n) =
    list.fold(mod.functions, #(c, env.new()), fn(acc, def) {
      let fun = def.definition
      let #(c, n) = acc
      let #(c, ref) = new_type_var_ref(c)
      let n = env.put(n, fun.name, Mono(TypeVar(ref)))
      #(c, n)
    })

  list.fold(mod.functions, c, fn(c, i) { infer_function(c, n, i) })
}

fn get_type_var(c: Context, var: TypeVarRef) {
  let assert Ok(x) = env.get(c.type_vars, var)
  x
}

fn set_type_var(c: Context, var: TypeVarRef, bind: TypeVar) {
  Context(..c, type_vars: env.put(c.type_vars, var, bind))
}

fn new_type_var_ref(c: Context) -> #(Context, TypeVarRef) {
  let ref = TypeVarRef(c.uid)
  let type_vars = env.put(c.type_vars, ref, Unbound(ref, 0))
  #(Context(..c, type_vars: type_vars, uid: c.uid + 1), ref)
}

fn infer_function(c: Context, n: ValueEnv, def: g.Definition(g.Function)) {
  let fun = def.definition
  let #(c, n) =
    list.fold(fun.parameters, #(c, n), fn(acc, param) {
      let #(c, n) = acc
      let name = case param.name {
        g.Named(s) -> s
        g.Discarded(s) -> s
      }
      let #(c, ref) = new_type_var_ref(c)
      let n = env.put(n, name, Poly(ref, Mono(TypeVar(ref))))
      #(c, n)
    })

  let #(c, e) = infer_body(c, n, fun.body)
  io.debug(e)
  c
}

fn infer_body(
  c: Context,
  n: ValueEnv,
  body: List(g.Statement),
) -> #(Context, Exp) {
  case body {
    [] -> panic as "empty body"
    [x] ->
      case x {
        g.Use(..) -> todo as "use statement"
        g.Assignment(value: exp, ..) -> infer_exp(c, n, exp)
        g.Expression(exp) -> infer_exp(c, n, exp)
      }
    [x, ..xs] ->
      case x {
        g.Use(..) -> todo as "use statement"
        g.Assignment(value: value, pattern: pattern, ..) -> {
          let #(c, value) = infer_exp(c, n, value)
          infer_bind_pattern(c, n, pattern, value, xs)
        }
        g.Expression(value) -> {
          let #(c, value) = infer_exp(c, n, value)
          let #(c, in) = infer_body(c, n, xs)
          #(c, Let(value.typ, "_", value, in))
        }
      }
  }
}

fn infer_bind_pattern(
  c: Context,
  n: ValueEnv,
  pattern: g.Pattern,
  value: Exp,
  body: List(g.Statement),
) -> #(Context, Exp) {
  todo
}

const int = TypeApp("Int", [])

fn infer_exp(c: Context, n: ValueEnv, exp: g.Expression) -> #(Context, Exp) {
  case exp {
    g.Int(s) -> #(c, Int(int, s))
    g.Variable(s) -> {
      // instantiate the poly type into a mono type
      let assert Ok(poly) = env.get(n, s)
      let #(c, typ) = instantiate(c, poly)
      #(c, Var(typ, s))
    }
    g.Call(fun, args) -> {
      // infer the type of the function
      let #(c, fun) = infer_exp(c, n, fun)

      // infer type of each arg
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, l) = acc
          let #(c, arg) = infer_exp(c, n, arg.item)
          #(c, [arg, ..l])
        })

      // new var for the return type
      let #(c, ret) = new_type_var_ref(c)
      let ret = TypeVar(ret)
      let arg_types = list.map(args, fn(x) { x.typ })

      // unify the actual function type with the types of args
      let c = unify(c, fun.typ, TypeFun(ret, arg_types))

      #(c, Call(ret, fun, args))
    }
    exp -> {
      io.debug(exp)
      todo as "not implemented"
    }
  }
}

fn is_bound(c: Context, a: Type) -> Bool {
  case a {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(a) -> True
        _ -> False
      }
    _ -> False
  }
}

fn occurs(c: Context, id: TypeVarRef, level: Int, in: Type) {
  todo
}

fn unify(c: Context, a: Type, b: Type) -> Context {
  let a_bound = is_bound(c, a)
  let b_bound = is_bound(c, b)
  case a, b, a_bound, b_bound {
    TypeVar(ref), b, True, _ -> {
      let assert Bound(a) = get_type_var(c, ref)
      unify(c, a, b)
    }
    a, TypeVar(ref), _, True -> {
      let assert Bound(b) = get_type_var(c, ref)
      unify(c, a, b)
    }
    TypeVar(ref), b, _, _ ->
      case a == b {
        True -> c
        False -> {
          let assert Unbound(aid, alevel) = get_type_var(c, ref)
          case occurs(c, aid, alevel, b) {
            True -> {
              io.debug(a)
              io.debug(b)
              panic as "recursive type"
            }
            False -> {
              set_type_var(c, ref, Bound(b))
            }
          }
        }
      }
    a, TypeVar(ref), _, _ ->
      case a == b {
        True -> c
        False -> {
          let assert Unbound(bid, blevel) = get_type_var(c, ref)
          case occurs(c, bid, blevel, a) {
            True -> {
              io.debug(a)
              io.debug(b)
              panic as "recursive type"
            }
            False -> {
              set_type_var(c, ref, Bound(a))
            }
          }
        }
      }
    TypeApp(aname, _), TypeApp(bname, _), _, _ if aname != bname -> {
      io.debug(a)
      io.debug(b)
      panic as "failed to unify types"
    }
    TypeApp(_, aargs), TypeApp(_, bargs), _, _ -> {
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          io.debug(a)
          io.debug(b)
          panic as "failed to unify types"
        }
      }
    }
    TypeFun(aret, aargs), TypeFun(bret, bargs), _, _ -> {
      let c = unify(c, aret, bret)
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          io.debug(a)
          io.debug(b)
          panic as "failed to unify types"
        }
      }
    }
    _, _, _, _ -> {
      io.debug(a)
      io.debug(b)
      panic as "failed to unify types"
    }
  }
}

fn instantiate(c: Context, poly: Poly) -> #(Context, Type) {
  do_instantiate(c, env.new(), poly)
}

fn do_instantiate(
  c: Context,
  n: Env(TypeVarRef, TypeVarRef),
  poly: Poly,
) -> #(Context, Type) {
  case poly {
    Mono(typ) -> #(c, do_instantiate_type(c, n, typ))
    Poly(var, poly) -> {
      let #(c, new_var) = new_type_var_ref(c)
      let n = env.put(n, var, new_var)
      do_instantiate(c, n, poly)
    }
  }
}

fn do_instantiate_type(
  c: Context,
  n: Env(TypeVarRef, TypeVarRef),
  typ: Type,
) -> Type {
  case typ {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> do_instantiate_type(c, n, x)
        Unbound(x, l) ->
          case env.get(n, x) {
            Ok(r) -> TypeVar(r)
            Error(_) -> typ
          }
      }
    TypeApp(name, args) ->
      TypeApp(name, list.map(args, do_instantiate_type(c, n, _)))
    TypeFun(fun, args) ->
      TypeFun(fun, list.map(args, do_instantiate_type(c, n, _)))
  }
}

fn generalise(t: Type) -> Poly {
  todo
}
