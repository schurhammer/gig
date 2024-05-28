import env.{type Env}
import glance as g

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None}

pub type VarName =
  String

pub type TypeVarRef {
  TypeVarRef(id: Int)
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
  Poly(vars: List(Int), typ: Type)
}

pub type TypeVar {
  Bound(Type)
  // TODO levels?
  Unbound(id: Int, level: Int)
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
    level: Int,
  )
}

const int = TypeApp("Int", [])

const bool = TypeApp("Bool", [])

fn get_id(a: Type) -> Int {
  let assert TypeVar(a) = a
  a.id
}

fn prelude(c: Context) -> #(Context, ValueEnv) {
  let n = env.new()

  let #(c, a) = new_type_var_ref(c)
  let n = env.put(n, "equal", Poly([get_id(a)], TypeFun(bool, [a, a])))

  let n = env.put(n, "add_int", Poly([get_id(a)], TypeFun(int, [int, int])))
  let n = env.put(n, "sub_int", Poly([get_id(a)], TypeFun(int, [int, int])))
  let n = env.put(n, "mul_int", Poly([get_id(a)], TypeFun(int, [int, int])))
  let n = env.put(n, "div_int", Poly([get_id(a)], TypeFun(int, [int, int])))

  #(c, n)
}

pub fn infer_module(mod: g.Module) {
  let c =
    Context(
      type_env: env.new(),
      type_vars: env.new(),
      functions: [],
      uid: 0,
      level: 0,
    )

  let #(c, n) = prelude(c)

  // TODO create env correctly
  let #(c, n) =
    list.fold(mod.functions, #(c, n), fn(acc, def) {
      let fun = def.definition
      let #(c, n) = acc
      let #(c, fun) = infer_function(c, n, def)
      let n = env.put(n, fun.name, fun.typ)
      #(c, n)
    })
}

fn get_type_var(c: Context, var: TypeVarRef) {
  let assert Ok(x) = env.get(c.type_vars, var)
  x
}

fn set_type_var(c: Context, var: TypeVarRef, bind: TypeVar) {
  Context(..c, type_vars: env.put(c.type_vars, var, bind))
}

fn new_type_var_ref(c: Context) -> #(Context, Type) {
  let ref = TypeVarRef(c.uid)
  let type_vars = env.put(c.type_vars, ref, Unbound(c.uid, c.level))
  #(Context(..c, type_vars: type_vars, uid: c.uid + 1), TypeVar(ref))
}

fn infer_function(
  c: Context,
  n: ValueEnv,
  def: g.Definition(g.Function),
) -> #(Context, Function) {
  // this function is a mix of let + abs

  let fun = def.definition

  // TODO before or after enter level?
  let #(c, t) = new_type_var_ref(c)
  let n = env.put(n, fun.name, Poly([], t))

  // enter let level
  let c = enter_level(c)

  // get list of param names
  let params =
    list.map(fun.parameters, fn(param) {
      case param.name {
        g.Named(s) -> s
        g.Discarded(s) -> s
      }
    })

  // create type vars for parameters
  let #(c, n, param_types) =
    list.fold(params, #(c, n, []), fn(acc, param) {
      let #(c, n, param_types) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = env.put(n, param, Poly([], typ))
      #(c, n, [typ, ..param_types])
    })
  let param_types = list.reverse(param_types)

  // infer body
  let #(c, body) = infer_body(c, n, fun.body)

  // exit let level
  let c = exit_level(c)

  // compute function type
  let typ = TypeFun(body.typ, param_types)
  let gen = generalise(c, typ)

  let fun = Function(fun.name, params, body, gen)

  env.debug(c.type_vars)
  io.debug(fun.name)
  io.debug(gen)

  #(c, fun)
}

fn enter_level(c: Context) -> Context {
  Context(..c, level: c.level + 1)
}

fn exit_level(c: Context) -> Context {
  Context(..c, level: c.level - 1)
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
          let c = enter_level(c)
          let #(c, value) = infer_exp(c, n, value)
          let c = exit_level(c)
          infer_bind_pattern(c, n, pattern, value, xs)
        }
        g.Expression(value) -> {
          let c = enter_level(c)
          let #(c, value) = infer_exp(c, n, value)
          let c = exit_level(c)
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

fn infer_exp(c: Context, n: ValueEnv, exp: g.Expression) -> #(Context, Exp) {
  case exp {
    g.Int(s) -> #(c, Int(int, s))
    g.Variable(s) -> {
      // instantiate the poly type into a mono type
      let poly = case env.get(n, s) {
        Ok(poly) -> poly
        Error(_) -> {
          io.debug(s)
          panic as "variable not in env"
        }
      }
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
      let arg_types = list.map(args, fn(x) { x.typ })

      // unify the actual function type with the types of args
      let c = unify(c, fun.typ, TypeFun(ret, arg_types))

      #(c, Call(ret, fun, args))
    }
    g.Fn(args, body, _) -> {
      // add parameters to the environment
      todo
    }
    g.BinaryOperator(name, left, right) -> {
      let name = case name {
        g.AddInt -> "add_int"
        g.SubInt -> "sub_int"
        g.MultInt -> "mul_int"
        g.DivInt -> "div_int"
        _ -> {
          io.debug(name)
          todo as "not implemented"
        }
      }
      let args = [g.Field(None, left), g.Field(None, right)]
      let fun = g.Call(g.Variable(name), args)
      infer_exp(c, n, fun)
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

fn occurs(c: Context, id: Int, level: Int, in: Type) -> #(Context, Bool) {
  case in {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(t) -> occurs(c, id, level, t)
        Unbound(i, l) -> {
          let min = int.min(l, level)
          let c = set_type_var(c, ref, Unbound(i, min))
          #(c, id == i)
        }
      }
    TypeApp(_, args) ->
      list.fold(args, #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, level, arg)
        #(c, b || b1)
      })
    TypeFun(fun, args) ->
      list.fold([fun, ..args], #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, level, arg)
        #(c, b || b1)
      })
  }
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
          let #(c, occurs) = occurs(c, aid, alevel, b)
          case occurs {
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
          let #(c, occurs) = occurs(c, bid, blevel, a)
          case occurs {
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
          panic as "incorrect number of type arguments"
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
          panic as "incorrect number of function arguments"
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

fn do_instantiate(c: Context, n: Env(Int, Type), poly: Poly) -> #(Context, Type) {
  let #(c, n) =
    list.fold(poly.vars, #(c, n), fn(acc, var) {
      let #(c, n) = acc
      let #(c, new_var) = new_type_var_ref(c)
      let n = env.put(n, var, new_var)
      #(c, n)
    })
  #(c, do_instantiate_type(c, n, poly.typ))
}

fn do_instantiate_type(c: Context, n: Env(Int, Type), typ: Type) -> Type {
  case typ {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> do_instantiate_type(c, n, x)
        Unbound(x, l) ->
          case env.get(n, x) {
            Ok(r) -> r
            Error(_) -> typ
          }
      }
    TypeApp(name, args) ->
      TypeApp(name, list.map(args, do_instantiate_type(c, n, _)))
    TypeFun(fun, args) ->
      TypeFun(
        do_instantiate_type(c, n, fun),
        list.map(args, do_instantiate_type(c, n, _)),
      )
  }
}

fn generalise(c: Context, t: Type) -> Poly {
  // TODO sort the list?
  let tvs = list.unique(find_tvs(c, t))
  Poly(tvs, t)
}

fn find_tvs(c: Context, t: Type) -> List(Int) {
  case t {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> find_tvs(c, x)
        Unbound(x, l) -> {
          case l > c.level {
            True -> [x]
            False -> []
          }
        }
      }
    TypeApp(_, args) -> list.flat_map(args, find_tvs(c, _))
    TypeFun(fun, args) -> list.flat_map([fun, ..args], find_tvs(c, _))
  }
}
