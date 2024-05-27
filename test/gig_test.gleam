import core.{
  type TypeVar, ExpAbs, ExpApp, ExpIf, ExpInt, ExpLet, ExpVar, Function, Module,
  Mono, Poly, TypeApp, TypeFun, TypeVar, normalize_vars_poly,
  normalize_vars_type, pretty_print_type,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should

fn normalize_type(t) {
  normalize_vars_type(t, dict.new()).0
}

fn normalize_poly(t) {
  normalize_vars_poly(t, dict.new()).0
}

fn infer_module(env: core.Env, module: core.Module) -> Result(core.Env, String) {
  use t <- result.try(core.w_module(env, module))

  list.fold(t.functions, env, fn(env, fun) {
    dict.insert(env, fun.name, fun.typ)
  })
  |> Ok
}

fn infer(env: core.Env, exp: core.Exp) -> Result(core.Type, String) {
  result.try(core.w(env, exp), fn(res) {
    let #(texpr, _sub) = res
    io.println_error("")
    io.println_error(
      texpr
      |> core.normalize_vars_texp(dict.new())
      |> fn(x) {
        let #(x, _) = x
        core.pretty_print_texp(x)
      },
    )
    Ok(texpr.typ)
  })
}

pub fn main() {
  gleeunit.main()
}

const bool = TypeApp("Bool", [])

const int = TypeApp("Int", [])

const true = #("True", Mono(bool))

const false = #("False", Mono(bool))

const add = #("+", Mono(TypeFun(int, [int, int])))

const sub = #("-", Mono(TypeFun(int, [int, int])))

const mul = #("*", Mono(TypeFun(int, [int, int])))

const eq = #("==", Poly(1, Mono(TypeFun(bool, [TypeVar(1), TypeVar(1)]))))

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
  |> should.equal(TypeFun(TypeVar(1), [TypeVar(1)]))
}

pub fn infer_app_test() {
  let env = dict.from_list([#("f", Mono(TypeFun(TypeVar(2), [TypeVar(1)])))])
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
      #("f", Mono(TypeFun(TypeVar(2), [TypeVar(1)]))),
      #("g", Mono(TypeFun(TypeVar(3), [TypeVar(2)]))),
    ])
  let exp = ExpApp(ExpVar("g"), [ExpApp(ExpVar("f"), [ExpVar("x")])])
  let result = infer(env, exp)
  result
  |> should.equal(Error("Unbound variable x"))
}

pub fn infer_poly_fail_1_test() {
  let env = dict.new()
  let id = ExpAbs(["x"], ExpVar("x"))
  let res =
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

  res
  |> should.equal(Error("Occurs check failed"))
}

pub fn infer_poly_fail_2_test() {
  let env = dict.new()
  let id = ExpAbs(["x"], ExpVar("x"))
  let res =
    infer(
      env,
      ExpLet(
        "id",
        id,
        ExpLet(
          "_",
          ExpApp(ExpVar("id"), [ExpInt(1)]),
          ExpApp(ExpVar("id"), [ExpVar("id")]),
        ),
      ),
    )

  res
  |> should.equal(Error("Types do not unify"))
}

pub fn infer_poly_fail_3_test() {
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
    TypeFun(TypeFun(TypeVar(2), [TypeVar(1)]), [
      TypeFun(TypeVar(2), [TypeVar(1)]),
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
    TypeFun(TypeVar(1), [TypeVar(1)])
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
    TypeFun(TypeFun(TypeVar(1), [TypeVar(2)]), [TypeVar(1)])
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
    TypeFun(
      TypeFun(TypeFun(TypeVar(2), [TypeVar(3)]), [
        TypeFun(TypeVar(1), [TypeVar(3)]),
      ]),
      [TypeFun(TypeVar(2), [TypeVar(1)])],
    )
    |> pretty_print_type(),
  )
}

pub fn infer_if_true_test() {
  let env = dict.from_list([true, false])
  let exp = ExpIf(ExpVar("True"), ExpInt(1), ExpInt(0))
  let assert Ok(result) = infer(env, exp)
  result
  |> should.equal(TypeApp("Int", []))
}

pub fn infer_if_false_test() {
  let env = dict.from_list([true, false])
  let exp = ExpIf(ExpVar("False"), ExpInt(1), ExpInt(0))
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
  let env = dict.from_list([true, false])
  let exp = ExpIf(ExpVar("True"), ExpInt(1), ExpVar("False"))
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
  |> should.equal(TypeFun(TypeVar(1), [TypeVar(1)]))
}

pub fn infer_multiple_args_test() {
  let env =
    dict.from_list([
      #("f", Mono(TypeFun(TypeVar(1), [TypeVar(2), TypeVar(3)]))),
      #("x", Mono(TypeVar(4))),
      #("y", Mono(TypeVar(5))),
    ])

  let exp = ExpApp(ExpVar("f"), [ExpVar("x"), ExpVar("y")])
  let assert Ok(result) = infer(env, exp)

  result
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeVar(1)
    |> pretty_print_type(),
  )
}

pub fn infer_zero_args_test() {
  let env = dict.from_list([#("f", Mono(TypeVar(1)))])

  let exp = ExpApp(ExpVar("f"), [])
  let assert Ok(result) = infer(env, exp)

  result
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeVar(1)
    |> pretty_print_type(),
  )
}

pub fn infer_multi_arg_function_def_test() {
  let env = dict.new()

  let func_def = ExpAbs(["x", "y"], ExpVar("x"))
  let assert Ok(func_type) = infer(env, func_def)

  func_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeFun(TypeVar(2), [TypeVar(2), TypeVar(1)])
    |> pretty_print_type(),
  )
}

pub fn infer_zero_arg_function_def_test() {
  let env = dict.new()

  let func_def = ExpAbs([], ExpInt(42))
  let assert Ok(func_type) = infer(env, func_def)

  func_type
  |> normalize_type()
  |> pretty_print_type()
  |> should.equal(
    TypeFun(TypeApp("Int", []), [])
    |> pretty_print_type(),
  )
}
