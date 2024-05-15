import gleam/dict
import gleam/list
import gleam/result

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

pub type Context =
  dict.Dict(ExpVar, Poly)

pub type Subs =
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

pub fn ftv_context(context: Context) -> List(TypeVar) {
  dict.fold(context, [], fn(acc, _, val) { list.append(acc, ftv_poly(val)) })
}

pub fn ftv_typing(context: Context, typ: Poly) -> List(TypeVar) {
  let context_vars = ftv_context(context)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(context_vars, x) })
}

@external(erlang, "erlang", "unique_integer")
pub fn new_type_var() -> TypeVar

pub fn unify(t1: Type, t2: Type) -> Result(Subs, String) {
  case t1, t2 {
    TypeVar(var1), TypeVar(var2) if var1 == var2 -> Ok(dict.new())
    TypeVar(var), typ -> unify_var(var, typ)
    typ, TypeVar(var) -> unify_var(var, typ)
    TypeApp(typ1, args1), TypeApp(typ2, args2) if typ1 == typ2 ->
      unify_many(args1, args2)
    _, _ -> Error("Types do not unify")
  }
}

fn unify_var(var: TypeVar, typ: Type) -> Result(Subs, String) {
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

pub fn apply_subs(subs: Subs, typ: Type) -> Type {
  dict.fold(subs, typ, fn(acc, var, replacement) {
    substitute(var, replacement, acc)
  })
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

pub fn unify_many(
  types1: List(Type),
  types2: List(Type),
) -> Result(Subs, String) {
  case types1, types2 {
    [head1, ..tail1], [head2, ..tail2] ->
      result.try(unify(head1, head2), fn(subs1) {
        let tail1 = list.map(tail1, fn(t) { apply_subs(subs1, t) })
        let tail2 = list.map(tail2, fn(t) { apply_subs(subs1, t) })
        result.try(unify_many(tail1, tail2), fn(subs2) {
          Ok(compose_subs(subs1, subs2))
        })
      })
    [], [] -> Ok(dict.new())
    _, _ -> Error("Type argument lists do not match")
  }
}

pub fn compose_subs(subs1: Subs, subs2: Subs) -> Subs {
  let subs2_applied =
    dict.map_values(subs2, fn(_, typ) { apply_subs(subs1, typ) })
  dict.fold(subs1, subs2_applied, fn(acc, var, typ) {
    dict.insert(acc, var, typ)
  })
}

pub fn generalize(context: Context, typ: Type) -> Poly {
  let vars = ftv_typing(context, Mono(typ))
  list.fold(vars, Mono(typ), fn(acc, var) { Poly(var, acc) })
}

pub fn instantiate(subs: Subs, poly: Poly) -> Type {
  case poly {
    Mono(typ) -> apply_subs(subs, typ)
    Poly(var, poly) -> {
      let new_var = new_type_var()
      let subs = dict.insert(subs, var, TypeVar(new_var))
      instantiate(subs, poly)
    }
  }
}

pub fn infer(context: Context, exp: Exp) -> Result(Type, String) {
  case exp {
    ExpVar(var) ->
      case dict.get(context, var) {
        Ok(poly) -> Ok(instantiate(dict.new(), poly))
        Error(_) -> Error("Unbound variable")
      }
    ExpApp(fun, arg) ->
      result.try(infer(context, fun), fn(fun_type) {
        result.try(infer(context, arg), fn(arg_type) {
          let var = TypeVar(new_type_var())
          let fun_type2 = TypeApp("->", [arg_type, var])
          result.try(unify(fun_type, fun_type2), fn(subs) {
            Ok(apply_subs(subs, var))
          })
        })
      })
    ExpAbs(var, body) -> {
      let var_type = TypeVar(new_type_var())
      let new_context = dict.insert(context, var, Mono(var_type))
      result.try(infer(new_context, body), fn(body_type) {
        Ok(TypeApp("->", [var_type, body_type]))
      })
    }
    ExpLet(var, val, body) ->
      result.try(infer(context, val), fn(val_type) {
        let new_context =
          dict.insert(context, var, generalize(context, val_type))
        infer(new_context, body)
      })
  }
}

pub fn normalize_type_vars(typ: Type) -> Type {
  let type_vars = list.unique(collect_type_vars(typ))
  let mapping = create_mapping(type_vars)
  apply_mapping(typ, mapping)
}

fn collect_type_vars(typ: Type) -> List(TypeVar) {
  case typ {
    TypeVar(var) -> [var]
    TypeApp(_, args) -> list.flat_map(args, collect_type_vars)
  }
}

fn create_mapping(type_vars: List(TypeVar)) -> dict.Dict(TypeVar, TypeVar) {
  list.index_fold(type_vars, dict.new(), fn(acc, var, index) {
    dict.insert(acc, var, index + 1)
  })
}

fn apply_mapping(typ: Type, mapping: dict.Dict(TypeVar, TypeVar)) -> Type {
  case typ {
    TypeVar(var) ->
      case dict.get(mapping, var) {
        Ok(new_var) -> TypeVar(new_var)
        Error(_) -> typ
        // This shouldn't happen as all vars should be in the mapping
      }
    TypeApp(name, args) -> {
      let new_args = list.map(args, fn(arg) { apply_mapping(arg, mapping) })
      TypeApp(name, new_args)
    }
  }
}
