import gleam/list

type ExpVar =
  String

type Exp {
  ExpVar(var: ExpVar)
  ExpApp(fun: Exp, arg: Exp)
  ExpAbs(var: ExpVar, exp: Exp)
  ExpLet(var: ExpVar, val: Exp, exp: Exp)
}

type TypeVar =
  Int

type Type {
  TypeVar(var: TypeVar)
  TypeApp(typ: String, args: List(Type))
}

type Poly {
  Mono(typ: Type)
  Poly(var: TypeVar, typ: Poly)
}

type Context =
  List(#(ExpVar, Poly))

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

fn ftv_context(context: Context) -> List(TypeVar) {
  list.flat_map(context, fn(x) { ftv_poly(x.1) })
}

fn ftv_typing(context: Context, typ: Poly) {
  let context = ftv_context(context)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(context, x) })
}

@external(erlang, "erlang", "unique_integer")
fn new_type_var() -> TypeVar

fn generalize(context: Context, typ: Type) -> Poly {
  let ftv = ftv_typing(context, Mono(typ))
  list.fold(ftv, Mono(typ), fn(typ, var) { Poly(var, typ) })
}

fn unify(typ1: Type, typ2: Type) {
  case typ1, typ2 {
    TypeVar(a), TypeVar(b) if a == b -> True
    TypeVar(a), typ -> unify_var(a, typ)
    typ, TypeVar(a) -> unify_var(a, typ)
    TypeApp(name1, args1), TypeApp(name2, args2) if name1 == name2 -> {
      case
        list.zip(args1, args2)
        |> list.all(fn(x) { unify(x.0, x.1) })
      {
        True -> True
        False -> todo
      }
    }
    _, _ -> todo
  }
}

fn unify_var(var: TypeVar, typ: Type) {
  // Occurs check
  case list.any(ftv(typ), fn(x) { x == var }) {
    True -> todo
    False -> {
      // Substitute the type variable with the type
      todo
    }
  }
}

fn w(exp: Exp, context: Context) -> Type {
  case exp {
    ExpVar(var) -> {
      // Lookup the type of the variable in the context
      // and return a fresh type variable if not found
      case list.find(context, fn(x) { x.0 == var }) {
        Ok(poly) -> Mono(poly.1)
        Error(_) -> TypeVar(new_type_var())
      }
    }
    ExpApp(fun, arg) -> {
      let fun_type = w(fun, context)
      let arg_type = w(arg, context)

      // Create a fresh type variable for the result type
      let res_type = TypeVar(new_type_var())

      // Unify the function type with (arg_type -> res_type)
      case unify(fun_type, TypeApp("->", [arg_type, res_type])) {
        True -> res_type
        False -> Error(TodoReason)
      }
    }
    ExpAbs(var, exp) -> {
      // Create a fresh type variable for the argument type
      let arg_type = TypeVar(new_type_var())

      // Infer the type of the body with the argument type in the context
      let body_type = w(exp, [#(var, Mono(arg_type)), ..context])

      // Return the function type (arg_type -> body_type)
      TypeApp("->", [arg_type, body_type])
    }
    ExpLet(var, val, exp) -> {
      let val_type = w(val, context)

      // Infer the type of the body with the variable and its type in the context
      let body_type = w(exp, [#(var, generalize(val_type, context)), ..context])

      body_type
    }
  }
}
