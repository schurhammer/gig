import gleam/list
import unique_integer

type Var =
  Int

pub type Type {
  TypeVar(id: Var)
  TypeFun(name: String, args: List(Type))
}

pub type Poly {
  Type(typ: Type)
  Poly(var: Var, typ: Poly)
}

pub type Env =
  List(#(String, Poly))

// TODO Type or Poly?
pub type Sub =
  List(#(Var, Type))

pub type Context {
  Context(env: Env, uid: Var)
}

fn new_var() {
  TypeVar(unique_integer.mono_positive())
}

fn ftv_type(typ: Type) -> List(Var) {
  case typ {
    TypeVar(a) -> [a]
    TypeFun(_, args) -> list.flat_map(args, ftv_type)
  }
}

fn ftv_poly(typ: Poly) -> List(Var) {
  case typ {
    Type(a) -> ftv_type(a)
    Poly(var, typ) ->
      ftv_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

fn ftv_env(env: Env) -> List(Var) {
  list.flat_map(env, fn(entry) { ftv_poly(entry.1) })
}

fn ftv_typing(env: Env, typ: Poly) -> List(Var) {
  let env_vars = ftv_env(env)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

fn replace_tv(in: Type, replace: Var, with: Type) -> Type {
  case in {
    TypeVar(v) if v == replace -> with
    TypeFun(name, args) ->
      TypeFun(name, list.map(args, replace_tv(_, replace, with)))
    _ -> in
  }
}

fn mgu(a: Type, b: Type) -> Sub {
  case a, b {
    TypeFun(a, _), TypeFun(b, _) if a != b -> panic as "failed unify - name"
    TypeFun(_, a), TypeFun(_, b) ->
      case list.strict_zip(a, b) {
        Ok(z) ->
          list.fold(z, [], fn(s, i) {
            let #(a, b) = i
            let s1 = mgu(apply_sub(s, a), apply_sub(s, b))
            compose_sub(s, s1)
          })
        Error(Nil) -> panic as "failed to unify - args"
      }
    TypeVar(a), TypeVar(b) if a == b -> []
    TypeVar(v), _ -> {
      let t = ftv_type(b)
      case list.contains(t, v) {
        True -> panic as "failed to unify - infinite type"
        False -> [#(v, b)]
      }
    }
    _, TypeVar(_) -> mgu(b, a)
  }
}

fn inst(typ: Poly) -> Type {
  case typ {
    Type(a) -> a
    Poly(a, typ) -> replace_tv(inst(typ), a, new_var())
  }
}

fn apply_sub(sub: Sub, in: Type) -> Type {
  list.fold(sub, in, fn(in, s) { replace_tv(in, s.0, s.1) })
}

fn compose_sub(s1: Sub, s2: Sub) -> Sub {
  // for each item in s1, apply s2 first
  let s1 = list.map(s1, fn(s) { #(s.0, apply_sub(s2, s.1)) })
  // find items that are in s2 but not s1, these remain unchanged
  let s2 =
    list.filter(s2, fn(s) {
      case list.find(s1, fn(x) { x.0 == s.0 }) {
        Ok(_) -> False
        _ -> True
      }
    })
  list.append(s1, s2)
}
