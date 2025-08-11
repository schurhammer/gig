// Imperative Monadic Form Implementation
// Based on "A Low-Level Look at A-Normal Form" Figure 10
//
// This module implements the imperative monadic form syntax and provides conversion
// from the monadic form to imperative monadic form.

import anf/monadic_form as monadic
import gig/core.{type LiteralKind}
import gig/mono.{type Type}
import gleam/list

// Value in imperative monadic form
pub type Value {
  // ι - constants/literals
  Literal(typ: Type, value: LiteralKind)
  // x - variables
  Var(typ: Type, name: String)
  // (op U⃗) - primitive operations (moved)
  Op(typ: Type, op: mono.Op, arguments: List(Value))
}

/// Statements in imperative monadic form
pub type Statement {
  Begin(statements: List(Statement))
  // (declare x T) - variable declaration with type
  Declare(variable: String, typ: Type)
  // (set! x v) - assignment of value to variable
  SetValue(variable: String, value: Value)
  // (set! x (op v⃗)) - assignment of operation result
  // SetOp(variable: String, op: mono.Op, arguments: List(Value))
  // (set! x (call v v⃗)) - assignment of direct call result
  SetCall(variable: String, function: Value, arguments: List(Value))
  // (set! x (call_closure v v⃗)) - assignment of closure call result
  SetCallClosure(variable: String, function: Value, arguments: List(Value))
  // (set! x t) - assignment of term to variable
  SetTerm(variable: String, term: Term)
  // (if0 v s s) - conditional statement
  If(condition: Value, then_stmt: Statement, else_stmt: Statement)
  // panic
  Panic(message: Value)
}

/// Terms in imperative monadic form
pub type Term {
  // v - value embedded as term
  Value(value: Value)
  // (op v⃗) - primitive operation term
  // Op(op: mono.Op, arguments: List(Value))
  // (call v v⃗) - direct function call term
  Call(function: Value, arguments: List(Value))
  // (call_closure v v⃗) - closure function call term
  CallClosure(function: Value, arguments: List(Value))
  // (begin s⃗ t) - sequence of statements followed by term
  BeginTerm(statements: List(Statement), term: Term)
  // (if0 v t t) - conditional term
  IfTerm(condition: Value, then_term: Term, else_term: Term)
  // panic
  PanicTerm(message: Value)
}

// Convert value to imperative form
fn mcg_value(value: monadic.Value) -> Value {
  case value {
    monadic.Literal(typ:, value:) -> Literal(typ, value)
    monadic.Var(typ:, name:) -> Var(typ, name)
    monadic.Op(typ:, op:, arguments:) ->
      Op(typ, op, list.map(arguments, mcg_value))
  }
}

/// Convert monadic form computation to imperative monadic form
pub fn mcg(comp: monadic.Computation) -> Term {
  case comp {
    monadic.Value(_, value) -> Value(mcg_value(value))
    monadic.Call(_, function, arguments) ->
      Call(mcg_value(function), list.map(arguments, mcg_value))
    monadic.CallClosure(_, function, arguments) ->
      CallClosure(mcg_value(function), list.map(arguments, mcg_value))
    monadic.Let(_, name, computation, body) -> {
      let comp_term = mcg(computation)
      let body_term = mcg(body)

      let assign_stmt = case comp_term {
        Value(val) -> SetValue(name, val)
        Call(func, args) -> SetCall(name, func, args)
        CallClosure(func, args) -> SetCallClosure(name, func, args)
        _ -> SetTerm(name, comp_term)
      }

      BeginTerm([Declare(name, computation.typ), assign_stmt], body_term)
    }
    monadic.If(_, condition, then_comp, else_comp) -> {
      let then_term = mcg(then_comp)
      let else_term = mcg(else_comp)
      IfTerm(mcg_value(condition), then_term, else_term)
    }
    monadic.Panic(_, message) -> PanicTerm(mcg_value(message))
  }
}
