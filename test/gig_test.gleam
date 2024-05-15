import gleeunit
import gleeunit/should

import gleam/dict
import lc.{
  ExpAbs, ExpApp, ExpLet, ExpVar, Mono, Poly, TypeApp, TypeVar, apply_subs,
  compose_subs, ftv, ftv_context, ftv_poly, ftv_typing, generalize, infer,
  instantiate, new_type_var, normalize_type_vars, occurs_check, substitute,
  unify, unify_many,
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

pub fn ftv_context_test() {
  let context =
    dict.from_list([#("x", Mono(TypeVar(1))), #("y", Poly(2, Mono(TypeVar(3))))])
  let result = ftv_context(context)
  result
  |> should.equal([1, 3])
}

pub fn ftv_typing_test() {
  let context =
    dict.from_list([#("x", Mono(TypeVar(1))), #("y", Poly(2, Mono(TypeVar(3))))])
  let poly = Poly(2, Mono(TypeVar(4)))
  let result = ftv_typing(context, poly)
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

pub fn apply_subs_test() {
  let subs = dict.from_list([#(1, TypeApp("List", [TypeVar(2)]))])
  let typ = TypeVar(1)
  let result = apply_subs(subs, typ)
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

pub fn compose_subs_test() {
  let subs1 = dict.from_list([#(1, TypeVar(2))])
  let subs2 = dict.from_list([#(2, TypeVar(3))])
  let result = compose_subs(subs1, subs2)
  result
  |> should.equal(dict.from_list([#(1, TypeVar(2)), #(2, TypeVar(3))]))
}

pub fn generalize_test() {
  let context = dict.from_list([#("x", Mono(TypeVar(1)))])
  let typ = TypeVar(2)
  let result = generalize(context, typ)
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
  let context = dict.from_list([#("x", Mono(TypeVar(1)))])
  let exp = ExpVar("x")
  let result = infer(context, exp)
  result
  |> should.equal(Ok(TypeVar(1)))
}

pub fn infer_abs_test() {
  let context = dict.new()
  let exp = ExpAbs("x", ExpVar("x"))
  let assert Ok(result) = infer(context, exp)
  result
  |> normalize_type_vars()
  |> should.equal(TypeApp("->", [TypeVar(1), TypeVar(1)]))
}

pub fn infer_app_test() {
  let context =
    dict.from_list([#("f", Mono(TypeApp("->", [TypeVar(1), TypeVar(2)])))])
  let exp = ExpApp(ExpVar("f"), ExpVar("x"))
  let result = infer(context, exp)
  result
  |> should.equal(Error("Unbound variable"))
}

pub fn infer_let_test() {
  let context = dict.new()
  let exp = ExpLet("x", ExpVar("y"), ExpVar("x"))
  let result = infer(context, exp)
  result
  |> should.equal(Error("Unbound variable"))
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
