import gleam/list

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

fn free_var_type(typ: Type) -> List(Var) {
  case typ {
    TypeVar(a) -> [a]
    TypeFun(_, args) -> list.flat_map(args, free_var_type)
  }
}

fn free_var_poly(typ: Poly) -> List(Var) {
  case typ {
    Type(a) -> free_var_type(a)
    Poly(var, typ) ->
      free_var_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

fn free_var_env(env: Env) -> List(Var) {
  list.flat_map(env, fn(entry) { free_var_poly(entry.1) })
}

fn free_var_typing(env: Env, typ: Poly) -> List(Var) {
  let env_vars = free_var_env(env)
  free_var_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

fn replace_type(in: Type, replace: Var, with: Type) -> Type {
  case in {
    TypeVar(v) if v == replace -> with
    TypeFun(name, args) ->
      TypeFun(name, list.map(args, replace_type(_, replace, with)))
    _ -> in
  }
}

fn apply_sub(sub: Sub, in: Type) -> Type {
  list.fold(sub, in, fn(in, s) { replace_type(in, s.0, s.1) })
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
