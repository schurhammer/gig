import gleeunit
import gleeunit/should

import gleam/dict
import gleam/int
import gleam/list
import gleam/string

import lc.{
  type Poly, type Type, type TypeVar, ExpAbs, ExpApp, ExpLet, ExpVar, Mono, Poly,
  TypeApp, TypeVar, apply_sub, compose_sub, ftv, ftv_env, ftv_poly, ftv_typing,
  generalize, infer, instantiate, occurs_check, substitute, unify, unify_many,
}

pub fn main() {
  gleeunit.main()
}

pub fn ftv_test() {
  let typ = TypeApp("List", [TypeVar(1)])
  let result = ftv(typ)
  result
  |> should.equal([1])
}

pub fn ftv_poly_test() {
  let poly = Poly(1, Mono(TypeVar(2)))
  let result = ftv_poly(poly)
  result
  |> should.equal([2])
}

pub fn ftv_env_test() {
  let env =
    dict.from_list([#("x", Mono(TypeVar(1))), #("y", Poly(2, Mono(TypeVar(3))))])
  let result = ftv_env(env)
  result
  |> should.equal([1, 3])
}

pub fn ftv_typing_test() {
  let env =
    dict.from_list([#("x", Mono(TypeVar(1))), #("y", Poly(2, Mono(TypeVar(3))))])
  let poly = Poly(2, Mono(TypeVar(4)))
  let result = ftv_typing(env, poly)
  result
  |> should.equal([4])
}

pub fn unify_test() {
  let type1 = TypeVar(1)
  let type2 = TypeVar(1)
  let result = unify(type1, type2)
  result
  |> should.equal(Ok(dict.new()))
}

pub fn occurs_check_test() {
  let typ = TypeApp("List", [TypeVar(1)])
  let result = occurs_check(1, typ)
  result
  |> should.equal(True)
}

pub fn apply_sub_test() {
  let subs = dict.from_list([#(1, TypeApp("List", [TypeVar(2)]))])
  let typ = TypeVar(1)
  let result = apply_sub(subs, typ)
  result
  |> should.equal(TypeApp("List", [TypeVar(2)]))
}

pub fn substitute_test() {
  let typ = TypeApp("List", [TypeVar(1)])
  let result = substitute(1, TypeVar(2), typ)
  result
  |> should.equal(TypeApp("List", [TypeVar(2)]))
}

pub fn unify_many_test() {
  let types1 = [TypeVar(1), TypeVar(2)]
  let types2 = [TypeVar(1), TypeVar(2)]
  let result = unify_many(types1, types2)
  result
  |> should.equal(Ok(dict.new()))
}

pub fn compose_sub_test() {
  let subs1 = dict.from_list([#(1, TypeVar(2))])
  let subs2 = dict.from_list([#(2, TypeVar(3))])
  let result = compose_sub(subs1, subs2)
  result
  |> should.equal(dict.from_list([#(1, TypeVar(2)), #(2, TypeVar(3))]))
}

pub fn generalize_test() {
  let env = dict.from_list([#("x", Mono(TypeVar(1)))])
  let typ = TypeVar(2)
  let result = generalize(env, typ)
  result
  |> should.equal(Poly(2, Mono(TypeVar(2))))
}

pub fn instantiate_test() {
  let poly = Poly(1, Mono(TypeVar(1)))
  let result = instantiate(dict.new(), poly)

  result
  |> normalize_type_vars()
  |> should.equal(TypeVar(1))
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

pub fn apply_sub_2_test() {
  let a = TypeVar(1)
  let b = TypeApp("->", [TypeVar(2), TypeVar(3)])
  let subs = dict.from_list([#(1, TypeApp("->", [TypeVar(2), TypeVar(3)]))])
  apply_sub(subs, a)
  |> should.equal(b)
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
