import gleam/list

pub type Type {
  TypeVar(id: Int)
  TypeFun(name: String, args: List(Type))
}

pub type PolyType {
  Type(typ: Type)
  PolyType(var: Int, typ: PolyType)
}

pub type Env =
  List(#(String, PolyType))

// TODO Type or PolyType?
pub type Sub =
  List(#(Int, Type))

pub type Context {
  Context(env: Env, uid: Int)
}

fn replace_type(in: Type, replace: Int, with: Type) -> Type {
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
