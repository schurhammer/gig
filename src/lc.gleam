import gleam/dict
import gleam/list
import gleam/result
import unique_integer

pub type ExpVar =
  String

pub type Exp {
  ExpVar(var: ExpVar)
  ExpApp(fun: Exp, arg: Exp)
  ExpAbs(var: ExpVar, exp: Exp)
  ExpLet(var: ExpVar, val: Exp, exp: Exp)
}

pub type TypeVar =
  Int

pub type Type {
  TypeVar(var: TypeVar)
  TypeApp(typ: String, args: List(Type))
}

pub type Poly {
  Mono(typ: Type)
  Poly(var: TypeVar, typ: Poly)
}

pub type Env =
  dict.Dict(ExpVar, Poly)

pub type Sub =
  dict.Dict(TypeVar, Type)

pub fn ftv(typ: Type) -> List(TypeVar) {
  case typ {
    TypeVar(a) -> [a]
    TypeApp(_, args) -> list.flat_map(args, ftv)
  }
}

pub fn ftv_poly(typ: Poly) -> List(TypeVar) {
  case typ {
    Mono(typ) -> ftv(typ)
    Poly(var, typ) ->
      ftv_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

pub fn ftv_env(env: Env) -> List(TypeVar) {
  dict.fold(env, [], fn(acc, _, val) { list.append(acc, ftv_poly(val)) })
}

pub fn ftv_typing(env: Env, typ: Poly) -> List(TypeVar) {
  let env_vars = ftv_env(env)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

pub fn new_type_var() -> TypeVar {
  unique_integer.mono_positive()
}

pub fn unify(t1: Type, t2: Type) -> Result(Sub, String) {
  case t1, t2 {
    TypeVar(var1), TypeVar(var2) if var1 == var2 -> Ok(dict.new())
    TypeVar(var), typ -> unify_var(var, typ)
    typ, TypeVar(var) -> unify_var(var, typ)
    TypeApp(typ1, args1), TypeApp(typ2, args2) if typ1 == typ2 ->
      unify_many(args1, args2)
    _, _ -> Error("Types do not unify")
  }
}

fn unify_var(var: TypeVar, typ: Type) -> Result(Sub, String) {
  case typ {
    TypeVar(var2) if var == var2 -> Ok(dict.new())
    _ ->
      case occurs_check(var, typ) {
        True -> Error("Occurs check failed")
        False -> Ok(dict.insert(dict.new(), var, typ))
      }
  }
}

pub fn occurs_check(var: TypeVar, typ: Type) -> Bool {
  list.contains(ftv(typ), var)
}

pub fn apply_sub(sub: Sub, typ: Type) -> Type {
  dict.fold(sub, typ, fn(acc, var, replacement) {
    substitute(var, replacement, acc)
  })
}

fn apply_sub_poly(sub: Sub, typ: Poly) -> Poly {
  case typ {
    Mono(t) -> Mono(apply_sub(sub, t))
    Poly(a, t) -> apply_sub_poly(dict.delete(sub, a), t)
  }
}

fn apply_sub_env(sub: Sub, env: Env) -> Env {
  dict.map_values(env, fn(_, v) { apply_sub_poly(sub, v) })
}

pub fn substitute(var: TypeVar, replacement: Type, typ: Type) -> Type {
  case typ {
    TypeVar(v) if v == var -> replacement
    TypeApp(name, args) ->
      TypeApp(
        name,
        list.map(args, fn(arg) { substitute(var, replacement, arg) }),
      )
    _ -> typ
  }
}

pub fn unify_many(types1: List(Type), types2: List(Type)) -> Result(Sub, String) {
  case types1, types2 {
    [head1, ..tail1], [head2, ..tail2] ->
      result.try(unify(head1, head2), fn(sub1) {
        let tail1 = list.map(tail1, fn(t) { apply_sub(sub1, t) })
        let tail2 = list.map(tail2, fn(t) { apply_sub(sub1, t) })
        result.try(unify_many(tail1, tail2), fn(sub2) {
          Ok(compose_sub(sub1, sub2))
        })
      })
    [], [] -> Ok(dict.new())
    _, _ -> Error("Type argument lists do not match")
  }
}

pub fn compose_sub(sub1: Sub, sub2: Sub) -> Sub {
  let sub2_applied = dict.map_values(sub2, fn(_, typ) { apply_sub(sub1, typ) })
  dict.fold(sub1, sub2_applied, fn(acc, var, typ) { dict.insert(acc, var, typ) })
}

pub fn generalize(env: Env, typ: Type) -> Poly {
  let vars = ftv_typing(env, Mono(typ))
  list.fold(vars, Mono(typ), fn(acc, var) { Poly(var, acc) })
}

pub fn instantiate(sub: Sub, poly: Poly) -> Type {
  case poly {
    Mono(typ) -> apply_sub(sub, typ)
    Poly(var, poly) -> {
      let new_var = new_type_var()
      let sub = dict.insert(sub, var, TypeVar(new_var))
      instantiate(sub, poly)
    }
  }
}

pub fn w(env: Env, exp: Exp) -> Result(#(Type, Sub), String) {
  case exp {
    ExpVar(var) ->
      case dict.get(env, var) {
        Ok(poly) -> {
          let typ = instantiate(dict.new(), poly)
          Ok(#(typ, dict.new()))
        }
        Error(_) -> Error("Unbound variable")
      }
    ExpApp(fun, arg) -> {
      let ret_type = TypeVar(new_type_var())
      use #(type1, sub1) <- result.try(w(env, fun))
      let env1 = apply_sub_env(sub1, env)
      use #(type2, sub2) <- result.try(w(env1, arg))
      let type1_sub = apply_sub(sub2, type1)
      let type3 = TypeApp("->", [type2, ret_type])
      use sub3 <- result.try(unify(type1_sub, type3))
      let sub = compose_sub(sub3, compose_sub(sub2, sub1))
      let ret_type = apply_sub(sub, ret_type)
      Ok(#(ret_type, sub))
    }
    ExpAbs(var, body) -> {
      let var_type = TypeVar(new_type_var())
      let new_env = dict.insert(env, var, Mono(var_type))
      use #(body_type, sub) <- result.try(w(new_env, body))
      let var_type = apply_sub(sub, var_type)
      let abs_type = TypeApp("->", [var_type, body_type])
      Ok(#(abs_type, sub))
    }
    ExpLet(var, val, body) -> {
      use #(type1, sub1) <- result.try(w(env, val))
      let type1_gen = generalize(apply_sub_env(sub1, env), type1)
      let env1 = dict.insert(env, var, type1_gen)
      let env1 = apply_sub_env(sub1, env1)
      use #(type2, sub2) <- result.try(w(env1, body))
      Ok(#(type2, compose_sub(sub1, sub2)))
    }
  }
}

pub fn infer(env: Env, exp: Exp) -> Result(Type, String) {
  result.try(w(env, exp), fn(res) {
    let #(typ, _sub) = res
    Ok(typ)
  })
}
