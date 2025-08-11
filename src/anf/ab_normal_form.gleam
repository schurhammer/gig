//// AB-Normal Form Implementation
//// Based on "A Low-Level Look at A-Normal Form" Figures 9, 11, and 20
////
//// This module implements AB-normalisation by applying the AB reduction rules
//// to imperative monadic form, then converting the result to AB-normal form.

import anf/imperative_form as imp
import gig/core
import gig/mono.{type Type}
import gleam/list

// AB-Normal Form Syntax from Figure 9:
// t ::= (begin s⃗ t) | v | (if0 v t t) | (call v v⃗) | (op v⃗)
// s ::= (begin s⃗) | (set! x v) | (set! x (op v⃗)) | (set! x (call v v⃗))
// v ::= (λ (x) t) | ι

// Value in AB-normal form
pub type Value {
  // ι - constants/literals
  Literal(typ: Type, value: core.LiteralKind)
  // x - variables
  Var(typ: Type, name: String)
  // (op U⃗) - primitive operations (moved)
  Op(typ: Type, op: mono.Op, arguments: List(Value))
}

/// Statements in AB-normal form
pub type Statement {
  // (begin s⃗) - sequence of statements
  Begin(statements: List(Statement))
  // variable declaration with type (implementation detail)
  Declare(variable: String, typ: Type)
  // (set! x v) - assignment of value to variable
  SetValue(variable: String, value: Value)
  // (set! x (op v⃗)) - assignment of operation result
  // SetOp(variable: String, op: mono.Op, arguments: List(Value))
  // (set! x (call v v⃗)) - assignment of direct call result
  SetCall(variable: String, function: Value, arguments: List(Value))
  // (set! x (call_closure v v⃗)) - assignment of closure call result
  SetCallClosure(variable: String, function: Value, arguments: List(Value))
  // (if0 v s s) - conditional statement
  If(condition: Value, then_stmt: Statement, else_stmt: Statement)
  // panic
  Panic(message: Value)
}

/// Terms in AB-normal form
/// Terms are always in the "return" position.
pub type Term {
  // v - value embedded as term
  Value(value: Value)
  // (call v v⃗) - direct function call term
  Call(function: Value, arguments: List(Value))
  // (call_closure v v⃗) - closure function call term
  CallClosure(function: Value, arguments: List(Value))
  // (op v⃗) - primitive operation term
  // Op(op: mono.Op, arguments: List(Value))
  // (begin s⃗ t) - sequence of statements followed by term
  BeginTerm(statements: List(Statement), term: Term)
  // (if0 v t t) - conditional term
  IfTerm(condition: Value, then_term: Term, else_term: Term)
  // panic
  PanicTerm(message: Value)
}

/// Convert imperative monadic value to AB-normal value
pub fn ab_normalise_value(value: imp.Value) -> Value {
  case value {
    imp.Literal(typ:, value:) -> Literal(typ:, value:)
    imp.Var(typ:, name:) -> Var(typ:, name:)
    imp.Op(typ:, op:, arguments:) ->
      Op(typ, op, list.map(arguments, ab_normalise_value))
  }
}

/// Convert imperative monadic term to AB-normal term
/// Implements abnf from Figure 20
pub fn ab_normalise(term: imp.Term) -> Term {
  case term {
    imp.BeginTerm(statements, term) -> {
      let statements = list.map(statements, ab_normalise_statement)
      let term = ab_normalise(term)
      BeginTerm(statements, term)
    }
    imp.IfTerm(condition:, then_term:, else_term:) -> {
      let condition = ab_normalise_value(condition)
      let then_term = ab_normalise(then_term)
      let else_term = ab_normalise(else_term)
      IfTerm(condition, then_term, else_term)
    }
    imp.Call(function:, arguments:) -> {
      let function = ab_normalise_value(function)
      let arguments = list.map(arguments, ab_normalise_value)
      Call(function, arguments)
    }
    imp.CallClosure(function:, arguments:) -> {
      let function = ab_normalise_value(function)
      let arguments = list.map(arguments, ab_normalise_value)
      CallClosure(function, arguments)
    }
    imp.Value(value:) -> {
      let value = ab_normalise_value(value)
      Value(value)
    }
    imp.PanicTerm(message:) -> {
      let message = ab_normalise_value(message)
      PanicTerm(message)
    }
  }
}

/// Convert imperative monadic statement to AB-normal statement
/// Implements abnf(_) from Figure 20, including AB-reduction rules
fn ab_normalise_statement(statement: imp.Statement) -> Statement {
  case statement {
    imp.Begin(statements) -> {
      let statements = list.map(statements, ab_normalise_statement)
      Begin(statements)
    }
    imp.Declare(variable, typ) -> Declare(variable, typ)
    imp.SetValue(variable, value) -> {
      let value = ab_normalise_value(value)
      SetValue(variable, value)
    }
    imp.SetCall(variable, function, arguments) -> {
      let function = ab_normalise_value(function)
      let arguments = list.map(arguments, ab_normalise_value)
      SetCall(variable, function, arguments)
    }
    imp.SetCallClosure(variable, function, arguments) -> {
      let function = ab_normalise_value(function)
      let arguments = list.map(arguments, ab_normalise_value)
      SetCallClosure(variable, function, arguments)
    }
    imp.If(condition, then_stmt, else_stmt) -> {
      let condition = ab_normalise_value(condition)
      let then_stmt = ab_normalise_statement(then_stmt)
      let else_stmt = ab_normalise_statement(else_stmt)
      If(condition, then_stmt, else_stmt)
    }
    // This handles the AB-reduction cases for (set! x t) where t is complex
    imp.SetTerm(variable, term) -> {
      case term {
        // AB₁: (set! x (if0 v t₁ t₂)) → (if0 v (set! x t₁) (set! x t₂))
        imp.IfTerm(condition, then_term, else_term) -> {
          let condition = ab_normalise_value(condition)
          let then_stmt =
            ab_normalise_statement(imp.SetTerm(variable, then_term))
          let else_stmt =
            ab_normalise_statement(imp.SetTerm(variable, else_term))
          If(condition, then_stmt, else_stmt)
        }
        // AB₂: (set! x (begin s⃗ t)) → (begin s⃗ (set! x t))
        imp.BeginTerm(statements, inner_term) -> {
          let statements = list.map(statements, ab_normalise_statement)
          let final_stmt =
            ab_normalise_statement(imp.SetTerm(variable, inner_term))
          Begin(list.append(statements, [final_stmt]))
        }
        // For other terms, convert to appropriate set form
        imp.Call(function, arguments) -> {
          let function = ab_normalise_value(function)
          let arguments = list.map(arguments, ab_normalise_value)
          SetCall(variable, function, arguments)
        }
        imp.CallClosure(function, arguments) -> {
          let function = ab_normalise_value(function)
          let arguments = list.map(arguments, ab_normalise_value)
          SetCallClosure(variable, function, arguments)
        }
        imp.Value(value) -> {
          let value = ab_normalise_value(value)
          SetValue(variable, value)
        }
        imp.PanicTerm(message:) -> {
          let message = ab_normalise_value(message)
          Panic(message)
        }
      }
    }
    imp.Panic(message:) -> {
      let message = ab_normalise_value(message)
      Panic(message)
    }
  }
}
