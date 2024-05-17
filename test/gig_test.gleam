import gleam/dict
import gleeunit
import gleeunit/should

import lc.{
  type TypeVar, ExpAbs, ExpApp, ExpBool, ExpIf, ExpInt, ExpLet, ExpVar, Mono,
  TypeApp, TypeVar, infer, normalize_vars_type, pretty_print_type,
}

fn normalize_type(t) {
  normalize_vars_type(t, dict.new()).0
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
  let exp = ExpAbs(["x"], ExpVar("x"))
  let assert Ok(result) = infer(env, exp)
  result
  |> normalize_type()
  |> should.equal(TypeApp("->", [TypeVar(1), TypeVar(1)]))
}

pub fn infer_app_test() {
  let env =
    dict.from_list([#("f", Mono(TypeApp("->", [TypeVar(2), TypeVar(1)])))])
  let exp = ExpApp(ExpVar("f"), [ExpVar("x")])
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable x"))
}

pub fn infer_let_test() {
  let env = dict.new()
  let exp = ExpLet("x", ExpVar("y"), ExpVar("x"))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable y"))
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
      ExpAbs(["x"], ExpVar("x")),
      ExpLet("a", ExpVar("f"), ExpApp(ExpVar("a"), [ExpVar("b")])),
    )
  let env = dict.from_list([#("b", Mono(TypeVar(1)))])
  let assert Ok(result) = infer(env, exp)
  result
  |> normalize_type()
  |> should.equal(TypeVar(1))
}

pub fn infer_function_composition_test() {
  let env =
    dict.from_list([
      #("f", Mono(TypeApp("->", [TypeVar(2), TypeVar(1)]))),
      #("g", Mono(TypeApp("->", [TypeVar(3), TypeVar(2)]))),
    ])
  let exp = ExpApp(ExpVar("g"), [ExpApp(ExpVar("f"), [ExpVar("x")])])
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable x"))
}

pub fn infer_poly_test() {
  let env = dict.new()
  let id = ExpAbs(["x"], ExpVar("x"))
  let assert Ok(id_type) =
    infer(
      env,
      ExpLet(
        "id",
        id,
        ExpApp(ExpApp(ExpVar("id"), [ExpVar("id")]), [
          ExpApp(ExpVar("id"), [ExpInt(1)]),
        ]),
      ),
    )

  id_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("Int", [])
    |> pretty_print_type(),
  )
}

pub fn infer_poly_fail_test() {
  let env = dict.new()
  // \id. id id 1 (\x.x)

  let id = ExpAbs(["x"], ExpVar("x"))
  let res =
    infer(
      env,
      ExpApp(
        ExpAbs(
          ["id"],
          ExpApp(ExpApp(ExpVar("id"), [ExpVar("id")]), [
            ExpApp(ExpVar("id"), [ExpInt(1)]),
          ]),
        ),
        [id],
      ),
    )

  res
  |> should.equal(Error("Occurs check failed"))
}

pub fn infer_higher_order_function_test() {
  let env = dict.new()
  let exp = ExpAbs(["f"], ExpAbs(["x"], ExpApp(ExpVar("f"), [ExpVar("x")])))
  let assert Ok(result) = infer(env, exp)

  result
  |> normalize_type()
  |> pretty_print_type
  |> should.equal(
    TypeApp("->", [
      TypeApp("->", [TypeVar(2), TypeVar(1)]),
      TypeApp("->", [TypeVar(2), TypeVar(1)]),
    ])
    |> pretty_print_type,
  )
}

pub fn infer_id_test() {
  let env = dict.new()

  let id = ExpAbs(["x"], ExpVar("x"))
  let assert Ok(id_type) = infer(env, id)

  id_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [TypeVar(1), TypeVar(1)])
    |> pretty_print_type(),
  )
}

pub fn infer_const_test() {
  let env = dict.new()

  let const_exp = ExpAbs(["x"], ExpAbs(["y"], ExpVar("x")))

  let assert Ok(const_type) = infer(env, const_exp)

  const_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [TypeApp("->", [TypeVar(1), TypeVar(2)]), TypeVar(1)])
    |> pretty_print_type(),
  )
}

pub fn infer_compose_test() {
  let env = dict.new()

  let compose =
    ExpAbs(
      ["f"],
      ExpAbs(
        ["g"],
        ExpAbs(["x"], ExpApp(ExpVar("f"), [ExpApp(ExpVar("g"), [ExpVar("x")])])),
      ),
    )
  let assert Ok(compose_type) = infer(env, compose)

  compose_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeApp("->", [
      TypeApp("->", [
        TypeApp("->", [TypeVar(2), TypeVar(3)]),
        TypeApp("->", [TypeVar(1), TypeVar(3)]),
      ]),
      TypeApp("->", [TypeVar(2), TypeVar(1)]),
    ])
    |> pretty_print_type(),
  )
}

pub fn infer_if_true_test() {
  let env = dict.new()
  let exp = ExpIf(ExpBool(True), ExpInt(1), ExpInt(0))
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeApp("Int", []))
}

pub fn infer_if_false_test() {
  let env = dict.new()
  let exp = ExpIf(ExpBool(False), ExpInt(1), ExpInt(0))
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeApp("Int", []))
}

pub fn infer_if_type_mismatch_test() {
  let env = dict.new()
  let exp = ExpIf(ExpInt(1), ExpInt(1), ExpInt(0))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Condition expression must be of type Bool"))
}

pub fn infer_if_branch_type_mismatch_test() {
  let env = dict.new()
  let exp = ExpIf(ExpBool(True), ExpInt(1), ExpBool(False))
  let result = infer(env, exp)
  result
  |> should.equal(Error("Types do not unify"))
}

pub fn infer_if_variable_test() {
  let env = dict.from_list([#("x", Mono(TypeApp("Bool", [])))])
  let exp = ExpIf(ExpVar("x"), ExpInt(1), ExpInt(0))
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeApp("Int", []))
}

pub fn infer_if_complex_test() {
  let env = dict.from_list([#("x", Mono(TypeApp("Bool", [])))])
  let exp =
    ExpIf(ExpVar("x"), ExpAbs(["y"], ExpVar("y")), ExpAbs(["z"], ExpVar("z")))
  let assert Ok(result) = infer(env, exp)
  result
  |> normalize_type()
  |> should.equal(TypeApp("->", [TypeVar(1), TypeVar(1)]))
}
