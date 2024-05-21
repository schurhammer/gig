import gleam/dict
import gleeunit
import gleeunit/should

import lc.{
  type TypeVar, ExpAbs, ExpApp, ExpIf, ExpInt, ExpLet, ExpVar, Function, Module,
  Mono, Poly, TypeApp, TypeVar, infer, infer_module, normalize_vars_poly,
  normalize_vars_type, pretty_print_type,
}

fn normalize_type(t) {
  normalize_vars_type(t, dict.new()).0
}

fn normalize_poly(t) {
  normalize_vars_poly(t, dict.new()).0
}

pub fn main() {
  gleeunit.main()
}

const bool = TypeApp("Bool", [])

const int = TypeApp("Int", [])

const true = #("True", Mono(bool))

const false = #("False", Mono(bool))

const add = #("+", Mono(TypeApp("->", [int, int, int])))

const sub = #("-", Mono(TypeApp("->", [int, int, int])))

const mul = #("*", Mono(TypeApp("->", [int, int, int])))

const eq = #("==", Poly(1, Mono(TypeApp("->", [bool, TypeVar(1), TypeVar(1)]))))

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
  |> should.equal(TypeApp("->", [TypeVar(1), TypeVar(1)]))
}

pub fn infer_multiple_args_test() {
  let env =
    dict.from_list([
      #("f", Mono(TypeApp("->", [TypeVar(1), TypeVar(2), TypeVar(3)]))),
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
    TypeApp("->", [TypeVar(2), TypeVar(2), TypeVar(1)])
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
    TypeApp("->", [TypeApp("Int", [])])
    |> pretty_print_type(),
  )
}

pub fn w_module_simple_function_test() {
  let env = dict.new()

  let functions = [
    Function("f", ExpAbs(["x"], ExpVar("x"))),
    Function("g", ExpAbs(["y"], ExpInt(42))),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(typ1) = dict.get(env, "f")
  let assert Ok(typ2) = dict.get(env, "g")

  typ1
  |> normalize_poly
  |> should.equal(Poly(
    1,
    Poly(1, Mono(TypeApp("->", [TypeVar(1), TypeVar(1)]))),
  ))

  typ2
  |> normalize_poly
  |> should.equal(Poly(1, Mono(TypeApp("->", [TypeApp("Int", []), TypeVar(1)]))))
}

pub fn w_module_mutually_recursive_functions_test() {
  let env = dict.from_list([eq, sub, true, false])

  let functions = [
    Function(
      "is_even",
      ExpAbs(
        ["n"],
        ExpIf(
          ExpApp(ExpVar("=="), [ExpVar("n"), ExpInt(0)]),
          ExpVar("True"),
          ExpApp(ExpVar("is_odd"), [
            ExpApp(ExpVar("-"), [ExpVar("n"), ExpInt(1)]),
          ]),
        ),
      ),
    ),
    Function(
      "is_odd",
      ExpAbs(
        ["n"],
        ExpIf(
          ExpApp(ExpVar("=="), [ExpVar("n"), ExpInt(0)]),
          ExpVar("False"),
          ExpApp(ExpVar("is_even"), [
            ExpApp(ExpVar("-"), [ExpVar("n"), ExpInt(1)]),
          ]),
        ),
      ),
    ),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(Mono(typ1)) = dict.get(env, "is_even")
  let assert Ok(Mono(typ2)) = dict.get(env, "is_odd")

  typ1
  |> normalize_type
  |> should.equal(TypeApp("->", [TypeApp("Bool", []), TypeApp("Int", [])]))

  typ2
  |> normalize_type
  |> should.equal(TypeApp("->", [TypeApp("Bool", []), TypeApp("Int", [])]))
}

pub fn w_module_simple_recursive_function_test() {
  let env = dict.from_list([add])

  let functions = [
    Function("f", ExpAbs(["x"], ExpApp(ExpVar("f"), [ExpVar("x")]))),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(typ) = dict.get(env, "f")

  typ
  |> normalize_poly
  |> should.equal(Poly(
    1,
    Poly(2, Mono(TypeApp("->", [TypeVar(2), TypeVar(1)]))),
  ))
}

pub fn w_module_function_arg_test() {
  let env = dict.from_list([add])

  let functions = [
    Function(
      "f",
      ExpAbs(["x"], ExpApp(ExpVar("+"), [ExpVar("x"), ExpVar("x")])),
    ),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(Mono(typ)) = dict.get(env, "f")

  typ
  |> normalize_type
  |> should.equal(TypeApp("->", [TypeApp("Int", []), TypeApp("Int", [])]))
}

pub fn w_module_recursive_test() {
  let env = dict.from_list([add])

  let functions = [
    Function(
      "f",
      ExpAbs(
        ["x"],
        ExpApp(ExpVar("f"), [ExpApp(ExpVar("+"), [ExpVar("x"), ExpVar("x")])]),
      ),
    ),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(typ) = dict.get(env, "f")

  typ
  |> normalize_poly
  |> should.equal(Poly(1, Mono(TypeApp("->", [TypeVar(1), TypeApp("Int", [])]))))
}

pub fn w_module_recursive_2_test() {
  let env = dict.from_list([add])

  let functions = [
    Function(
      "f",
      ExpAbs(
        ["x"],
        ExpApp(ExpVar("+"), [ExpApp(ExpVar("f"), [ExpVar("x")]), ExpVar("x")]),
      ),
    ),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(Mono(typ)) = dict.get(env, "f")

  typ
  |> normalize_type
  |> should.equal(TypeApp("->", [TypeApp("Int", []), TypeApp("Int", [])]))
}

pub fn w_module_recursive_3_test() {
  let env = dict.from_list([])

  let functions = [
    Function("f", ExpAbs(["x"], ExpApp(ExpVar("g"), [ExpVar("x")]))),
    Function("g", ExpAbs(["x"], ExpApp(ExpVar("f"), [ExpVar("x")]))),
  ]
  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(typ1) = dict.get(env, "f")
  let assert Ok(typ2) = dict.get(env, "g")

  typ1
  |> normalize_poly
  |> should.equal(Poly(
    1,
    Poly(2, Mono(TypeApp("->", [TypeVar(2), TypeVar(1)]))),
  ))
  typ2
  |> normalize_poly
  |> should.equal(Poly(
    1,
    Poly(2, Mono(TypeApp("->", [TypeVar(2), TypeVar(1)]))),
  ))
}

pub fn w_module_recursive_function_test() {
  let env = dict.from_list([eq, sub, mul])

  let functions = [
    Function(
      "fact",
      ExpAbs(
        ["n"],
        ExpIf(
          ExpApp(ExpVar("=="), [ExpVar("n"), ExpInt(0)]),
          ExpInt(1),
          ExpApp(ExpVar("*"), [
            ExpVar("n"),
            ExpApp(ExpVar("fact"), [
              ExpApp(ExpVar("-"), [ExpVar("n"), ExpInt(1)]),
            ]),
          ]),
        ),
      ),
    ),
  ]

  let module = Module(functions)

  let assert Ok(env) = infer_module(env, module)
  let assert Ok(Mono(typ)) = dict.get(env, "fact")

  typ
  |> normalize_type
  |> should.equal(TypeApp("->", [TypeApp("Int", []), TypeApp("Int", [])]))
}
