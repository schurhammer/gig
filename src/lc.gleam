import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import unique_integer

pub type ExpVar =
  String

pub type Exp {
  ExpBool(val: Bool)
  ExpInt(val: Int)
  ExpVar(var: ExpVar)
  ExpApp(fun: Exp, arg: Exp)
  ExpAbs(var: ExpVar, exp: Exp)
  ExpLet(var: ExpVar, val: Exp, exp: Exp)
}

pub type TypeVar =
  Int

pub type TExp {
  TExpBool(typ: Type, val: Bool)
  TExpInt(typ: Type, val: Int)
  TExpVar(typ: Type, var: ExpVar)
  TExpApp(typ: Type, fun: TExp, arg: TExp)
  TExpAbs(typ: Type, var: ExpVar, exp: TExp)
  TExpLet(typ: Type, var: ExpVar, val: TExp, exp: TExp)
}

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

fn ftv(typ: Type) -> List(TypeVar) {
  case typ {
    TypeVar(a) -> [a]
    TypeApp(_, args) -> list.flat_map(args, ftv)
  }
}

fn ftv_poly(typ: Poly) -> List(TypeVar) {
  case typ {
    Mono(typ) -> ftv(typ)
    Poly(var, typ) ->
      ftv_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

fn ftv_env(env: Env) -> List(TypeVar) {
  dict.fold(env, [], fn(acc, _, val) { list.append(acc, ftv_poly(val)) })
}

fn ftv_typing(env: Env, typ: Poly) -> List(TypeVar) {
  let env_vars = ftv_env(env)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

fn new_type_var() -> TypeVar {
  unique_integer.mono_positive()
}

fn unify(t1: Type, t2: Type) -> Result(Sub, String) {
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
      case occurs(var, typ) {
        True -> Error("Occurs check failed")
        False -> Ok(dict.insert(dict.new(), var, typ))
      }
  }
}

fn occurs(var: TypeVar, typ: Type) -> Bool {
  list.contains(ftv(typ), var)
}

fn unify_many(types1: List(Type), types2: List(Type)) -> Result(Sub, String) {
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

fn apply_sub(sub: Sub, typ: Type) -> Type {
  case typ {
    TypeVar(v) ->
      case dict.get(sub, v) {
        Ok(r) -> r
        _ -> typ
      }
    TypeApp(name, args) -> TypeApp(name, list.map(args, apply_sub(sub, _)))
  }
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

fn apply_sub_texpr(sub: Sub, texp: TExp) -> TExp {
  case texp {
    TExpBool(_, _) -> texp
    TExpInt(_, _) -> texp
    TExpVar(typ, var) -> TExpVar(apply_sub(sub, typ), var)
    TExpApp(typ, fun, arg) ->
      TExpApp(
        apply_sub(sub, typ),
        apply_sub_texpr(sub, fun),
        apply_sub_texpr(sub, arg),
      )
    TExpAbs(typ, var, exp) ->
      TExpAbs(apply_sub(sub, typ), var, apply_sub_texpr(sub, exp))
    TExpLet(typ, var, val, exp) ->
      TExpLet(
        apply_sub(sub, typ),
        var,
        apply_sub_texpr(sub, val),
        apply_sub_texpr(sub, exp),
      )
  }
}

fn compose_sub(sub1: Sub, sub2: Sub) -> Sub {
  let sub2_applied = dict.map_values(sub2, fn(_, typ) { apply_sub(sub1, typ) })
  dict.fold(sub1, sub2_applied, fn(acc, var, typ) { dict.insert(acc, var, typ) })
}

fn gen(env: Env, typ: Type) -> Poly {
  let vars = ftv_typing(env, Mono(typ))
  list.fold(vars, Mono(typ), fn(acc, var) { Poly(var, acc) })
}

fn inst(sub: Sub, poly: Poly) -> Type {
  case poly {
    Mono(typ) -> apply_sub(sub, typ)
    Poly(var, poly) -> {
      let new_var = new_type_var()
      let sub = dict.insert(sub, var, TypeVar(new_var))
      inst(sub, poly)
    }
  }
}

pub fn w(env: Env, exp: Exp) -> Result(#(TExp, Sub), String) {
  case exp {
    ExpBool(val) -> Ok(#(TExpBool(TypeApp("Bool", []), val), dict.new()))
    ExpInt(val) -> Ok(#(TExpInt(TypeApp("Int", []), val), dict.new()))
    ExpVar(var) ->
      case dict.get(env, var) {
        Ok(poly) -> {
          let typ = inst(dict.new(), poly)
          Ok(#(TExpVar(typ, var), dict.new()))
        }
        Error(_) -> Error("Unbound variable")
      }
    ExpApp(fun, arg) -> {
      let ret_type = TypeVar(new_type_var())
      use #(texp1, sub1) <- result.try(w(env, fun))
      let env1 = apply_sub_env(sub1, env)
      use #(texp2, sub2) <- result.try(w(env1, arg))
      let texp1_sub2 = apply_sub_texpr(sub2, texp1)
      let type3 = TypeApp("->", [texp2.typ, ret_type])
      use sub3 <- result.try(unify(texp1_sub2.typ, type3))
      let sub123 = compose_sub(sub3, compose_sub(sub2, sub1))
      let ret_type = apply_sub(sub123, ret_type)
      let texp1_sub3 = apply_sub_texpr(sub3, texp1_sub2)
      let texp2_sub3 = apply_sub_texpr(sub3, texp2)
      Ok(#(TExpApp(ret_type, texp1_sub3, texp2_sub3), sub123))
    }
    ExpAbs(var, body) -> {
      let var_type = TypeVar(new_type_var())
      let body_env = dict.insert(env, var, Mono(var_type))
      use #(body_texp, sub) <- result.try(w(body_env, body))
      let var_type = apply_sub(sub, var_type)
      let abs_type = TypeApp("->", [var_type, body_texp.typ])
      Ok(#(TExpAbs(abs_type, var, body_texp), sub))
    }
    ExpLet(var, val, body) -> {
      use #(texp1, sub1) <- result.try(w(env, val))
      let type1_gen = gen(apply_sub_env(sub1, env), texp1.typ)
      let env1 = dict.insert(env, var, type1_gen)
      // TODO this line seems to break let polymorphism
      // let env1 = apply_sub_env(sub1, env1)
      use #(texp2, sub2) <- result.try(w(env1, body))
      Ok(#(TExpLet(texp2.typ, var, texp1, texp2), compose_sub(sub1, sub2)))
    }
  }
}

pub fn infer(env: Env, exp: Exp) -> Result(Type, String) {
  result.try(w(env, exp), fn(res) {
    let #(texpr, _sub) = res
    Ok(texpr.typ)
  })
}

pub fn pretty_print_type(typ: Type) -> String {
  case typ {
    TypeVar(var) -> format_type_var(var)
    TypeApp(name, args) -> {
      case name == "->", args {
        True, [head, tail] -> format_function_type(head, tail)
        False, _ -> format_type_app(name, args)
        _, _ -> "<err>"
      }
    }
  }
}

pub fn pretty_print_exp(exp: Exp) -> String {
  case exp {
    ExpBool(val) -> bool.to_string(val)
    ExpInt(val) -> int.to_string(val)
    ExpVar(var) -> var
    ExpApp(fun, arg) ->
      "(" <> pretty_print_exp(fun) <> " " <> pretty_print_exp(arg) <> ")"
    ExpAbs(var, exp) -> "(λ" <> var <> ". " <> pretty_print_exp(exp) <> ")"
    ExpLet(var, val, exp) ->
      "(let "
      <> var
      <> " = "
      <> pretty_print_exp(val)
      <> " in "
      <> pretty_print_exp(exp)
      <> ")"
  }
}

pub fn pretty_print_texp(texp: TExp) -> String {
  case texp {
    TExpBool(_, val) -> bool.to_string(val)
    TExpInt(_, val) -> int.to_string(val)
    TExpVar(_, var) -> var
    TExpApp(typ, fun, arg) ->
      "("
      <> pretty_print_texp(fun)
      <> " "
      <> pretty_print_texp(arg)
      <> ")"
      <> ": "
      <> pretty_print_type(typ)
    TExpAbs(typ, var, exp) ->
      "(λ"
      <> var
      <> ". "
      <> pretty_print_texp(exp)
      <> ")"
      <> ": "
      <> pretty_print_type(typ)
    TExpLet(_, var, val, exp) ->
      "(let "
      <> var
      <> " = "
      <> pretty_print_texp(val)
      <> " in "
      <> pretty_print_texp(exp)
      <> ")"
  }
}

pub fn normalize_vars_type(
  typ: Type,
  sub: dict.Dict(TypeVar, TypeVar),
) -> #(Type, dict.Dict(TypeVar, TypeVar)) {
  case typ {
    TypeVar(var) -> {
      case dict.get(sub, var) {
        Ok(new_var) -> #(TypeVar(new_var), sub)
        Error(_) -> {
          let new_var = dict.size(sub) + 1
          let new_sub = dict.insert(sub, var, new_var)
          #(TypeVar(new_var), new_sub)
        }
      }
    }
    TypeApp(name, args) -> {
      let result =
        args
        |> list.fold(#([], sub), fn(acc, arg) {
          let #(l, sub) = acc
          let #(new_arg, new_sub) = normalize_vars_type(arg, sub)
          #([new_arg, ..l], new_sub)
        })
      #(
        TypeApp(
          name,
          result.0
            |> list.reverse(),
        ),
        result.1,
      )
    }
  }
}

pub fn normalize_vars_texp(
  texp: TExp,
  sub: dict.Dict(TypeVar, TypeVar),
) -> #(TExp, dict.Dict(TypeVar, TypeVar)) {
  case texp {
    TExpBool(typ, val) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      #(TExpBool(new_typ, val), new_sub)
    }
    TExpInt(typ, val) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      #(TExpInt(new_typ, val), new_sub)
    }
    TExpVar(typ, var) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      #(TExpVar(new_typ, var), new_sub)
    }
    TExpApp(typ, fun, arg) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_fun, new_sub) = normalize_vars_texp(fun, new_sub)
      let #(new_arg, new_sub) = normalize_vars_texp(arg, new_sub)
      #(TExpApp(new_typ, new_fun, new_arg), new_sub)
    }
    TExpAbs(typ, var, exp) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_exp, new_sub) = normalize_vars_texp(exp, new_sub)
      #(TExpAbs(new_typ, var, new_exp), new_sub)
    }
    TExpLet(typ, var, val, exp) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_val, new_sub) = normalize_vars_texp(val, new_sub)
      let #(new_exp, new_sub) = normalize_vars_texp(exp, new_sub)
      #(TExpLet(new_typ, var, new_val, new_exp), new_sub)
    }
  }
}

fn format_type_var(var: TypeVar) -> String {
  let ascii = 96 + var
  let assert Ok(s) = bit_array.to_string(<<ascii:int>>)
  s
}

fn format_function_type(arg: Type, result: Type) -> String {
  let arg_str = case arg {
    TypeApp("->", _) -> "(" <> pretty_print_type(arg) <> ")"
    _ -> pretty_print_type(arg)
  }
  let res_str = pretty_print_type(result)
  arg_str <> " -> " <> res_str
}

fn format_type_app(name: String, args: List(Type)) -> String {
  let args_str =
    args
    |> list.map(pretty_print_type)
    |> string.join(" ")
  case args_str {
    "" -> name
    s -> name <> " " <> s
  }
}
