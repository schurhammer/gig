import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

import lc.{
  type Poly, type Type, type TypeVar, ExpAbs, ExpApp, ExpInt, ExpLet, ExpVar,
  Mono, Poly, TypeApp, TypeVar, infer,
}

pub fn main() {
  gleeunit.main()
}

pub fn infer_var_test() {
  let env = dict.from_list([#("x", Mono(TypeVar(1)))])
  let exp = ExpVar("x")
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeVar(1))
}

pub fn infer_abs_test() {
  let env = dict.new()
  let exp = ExpAbs("x", ExpVar("x"))
  let assert Ok(result) = infer(env, exp)
  result
  |> normalize_type_vars()
  |> should.equal(TypeApp("->", [TypeVar(1), TypeVar(1)]))
}

pub fn infer_app_test() {
  let env =
    dict.from_list([#("f", Mono(TypeApp("->", [TypeVar(1), TypeVar(2)])))])
  let exp = ExpApp(ExpVar("f"), ExpVar("x"))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable"))
}

pub fn infer_let_test() {
  let env = dict.new()
  let exp = ExpLet("x", ExpVar("y"), ExpVar("x"))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable"))
}

pub fn infer_let_bound_var_test() {
  let env = dict.from_list([#("y", Mono(TypeVar(1)))])
  let exp = ExpLet("x", ExpVar("y"), ExpVar("x"))
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeVar(1))
}

pub fn infer_nested_let_test() {
  let exp =
    ExpLet(
      "f",
      ExpAbs("x", ExpVar("x")),
      ExpLet("a", ExpVar("f"), ExpApp(ExpVar("a"), ExpVar("b"))),
    )
  let env = dict.from_list([#("b", Mono(TypeVar(1)))])
  let assert Ok(result) = infer(env, exp)
  result
  |> normalize_type_vars()
  |> should.equal(TypeVar(1))
}

pub fn infer_function_composition_test() {
  let env =
    dict.from_list([
      #("f", Mono(TypeApp("->", [TypeVar(1), TypeVar(2)]))),
      #("g", Mono(TypeApp("->", [TypeVar(2), TypeVar(3)]))),
    ])
  let exp = ExpApp(ExpVar("g"), ExpApp(ExpVar("f"), ExpVar("x")))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable"))
}

pub fn infer_poly_test() {
  let env = dict.new()
  let id = ExpAbs("x", ExpVar("x"))
  let assert Ok(id_type) =
    infer(
      env,
      ExpLet(
        "id",
        id,
        ExpApp(
          ExpApp(ExpVar("id"), ExpVar("id")),
          ExpApp(ExpVar("id"), ExpInt(1)),
        ),
      ),
    )

  id_type
  |> normalize_type_vars()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("Int", [])
    |> pretty_print_type(),
  )
}

pub fn infer_poly_fail_test() {
  let env = dict.new()
  // \id. id id 1 (\x.x)

  let id = ExpAbs("x", ExpVar("x"))
  let res =
    infer(
      env,
      ExpApp(
        ExpAbs(
          "id",
          ExpApp(
            ExpApp(ExpVar("id"), ExpVar("id")),
            ExpApp(ExpVar("id"), ExpInt(1)),
          ),
        ),
        id,
      ),
    )

  res
  |> should.equal(Error("Occurs check failed"))
}

pub fn infer_higher_order_function_test() {
  let env = dict.new()
  let exp = ExpAbs("f", ExpAbs("x", ExpApp(ExpVar("f"), ExpVar("x"))))
  let assert Ok(result) = infer(env, exp)

  result
  |> normalize_type_vars()
  |> pretty_print_type
  |> should.equal(
    TypeApp("->", [
      TypeApp("->", [TypeVar(1), TypeVar(2)]),
      TypeApp("->", [TypeVar(1), TypeVar(2)]),
    ])
    |> pretty_print_type,
  )
}

pub fn infer_id_test() {
  let env = dict.new()

  let id = ExpAbs("x", ExpVar("x"))
  let assert Ok(id_type) = infer(env, id)

  id_type
  |> normalize_type_vars()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [TypeVar(1), TypeVar(1)])
    |> pretty_print_type(),
  )
}

pub fn infer_const_test() {
  let env = dict.new()

  let const_exp = ExpAbs("x", ExpAbs("y", ExpVar("x")))

  let assert Ok(const_type) = infer(env, const_exp)

  const_type
  |> normalize_type_vars()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [TypeVar(1), TypeApp("->", [TypeVar(2), TypeVar(1)])])
    |> pretty_print_type(),
  )
}

pub fn infer_compose_test() {
  let env = dict.new()

  let compose =
    ExpAbs(
      "f",
      ExpAbs(
        "g",
        ExpAbs("x", ExpApp(ExpVar("f"), ExpApp(ExpVar("g"), ExpVar("x")))),
      ),
    )
  let assert Ok(compose_type) = infer(env, compose)

  compose_type
  |> normalize_type_vars()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [
      TypeApp("->", [TypeVar(2), TypeVar(3)]),
      TypeApp("->", [
        TypeApp("->", [TypeVar(1), TypeVar(2)]),
        TypeApp("->", [TypeVar(1), TypeVar(3)]),
      ]),
    ])
    |> normalize_type_vars()
    |> pretty_print_type(),
  )
}

pub fn normalize_type_vars_test() {
  let typ =
    TypeApp("Pair", [TypeVar(42), TypeApp("List", [TypeVar(7)]), TypeVar(42)])
  let normalized_typ = normalize_type_vars(typ)
  let expected_typ =
    TypeApp("Pair", [TypeVar(1), TypeApp("List", [TypeVar(2)]), TypeVar(1)])

  normalized_typ
  |> should.equal(expected_typ)
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

fn format_type_var(var: TypeVar) -> String {
  "a" <> int.to_string(var)
}

fn pretty_print_poly(poly: Poly) -> String {
  case poly {
    Mono(typ) -> pretty_print_type(typ)
    Poly(var, typ) -> {
      let var_str = format_type_var(var)
      let typ_str = pretty_print_poly(typ)
      "forall " <> var_str <> ". " <> typ_str
    }
  }
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

fn normalize_type_vars(typ: Type) -> Type {
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
      }
    TypeApp(name, args) -> {
      let new_args = list.map(args, fn(arg) { apply_mapping(arg, mapping) })
      TypeApp(name, new_args)
    }
  }
}
