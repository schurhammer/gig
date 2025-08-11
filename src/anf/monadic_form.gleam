// Monadic Form Implementation
// Based on "A Low-Level Look at A-Normal Form" Figure 3
//
// This module implements the monadic form syntax and provides conversion
// from the closure Exp type to monadic form.

import gig/closure
import gig/core.{type LiteralKind}
import gig/mono.{type Type}
import gleam/int
import gleam/list
import gleam/option.{None, Some}

// Monadic Form Syntax from Figure 3:
// Values U ::= ι | x | (λ (x) C)
// Computations C ::= U | (U U⃗) | (op U⃗) | (let (x C) C) | (if0 U C C)

/// Values in monadic form - these are pure and cannot have effects
pub type Value {
  // ι - constants/literals
  Literal(typ: Type, value: LiteralKind)
  // x - variables
  Var(typ: Type, name: String)
  // (op U⃗) - primitive operations (moved from computations)
  Op(typ: Type, op: mono.Op, arguments: List(Value))
}

/// Computations in monadic form - these may have effects
pub type Computation {
  // U - values embedded as computations (monadic return)
  Value(typ: Type, value: Value)
  // (U U⃗) - direct function calls
  Call(typ: Type, function: Value, arguments: List(Value))
  // (U U⃗) - closure function calls
  CallClosure(typ: Type, function: Value, arguments: List(Value))
  // (op U⃗) - primitive operations
  // Op(typ: Type, op: mono.Op, arguments: List(Value))
  // (let (x C) C) - monadic bind
  Let(typ: Type, name: String, computation: Computation, body: Computation)
  // (if0 U C C) - conditional on values
  If(
    typ: Type,
    condition: Value,
    then_branch: Computation,
    else_branch: Computation,
  )
  Panic(typ: Type, arg: Value)
}

pub type Context {
  Context(counter: Int)
}

fn new_context() -> Context {
  Context(counter: 0)
}

/// Generate a fresh temporary variable name and update context
fn fresh_temp(ctx: Context) -> #(Context, String) {
  let name = "T" <> int.to_string(ctx.counter)
  let new_ctx = Context(counter: ctx.counter + 1)
  #(new_ctx, name)
}

/// Convert a closure Exp to monadic form
pub fn mf(exp: closure.Exp) -> Computation {
  let ctx = new_context()
  let #(_, result) = do_mf(ctx, exp)
  result
}

fn unwrap_value(c: Context, exp: closure.Exp) -> Result(#(Context, Value), Nil) {
  case exp {
    closure.Literal(typ:, val:) -> Ok(#(c, Literal(typ:, value: val)))
    closure.Var(typ:, var:) -> Ok(#(c, Var(typ:, name: var)))
    closure.Op(..) ->
      case do_mf(c, exp) {
        #(c, Value(_, val)) -> Ok(#(c, val))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn do_mf(ctx: Context, exp: closure.Exp) -> #(Context, Computation) {
  case exp {
    closure.Literal(typ:, val:) -> #(ctx, Value(typ, Literal(typ:, value: val)))
    closure.Var(typ:, var:) -> #(ctx, Value(typ, Var(typ:, name: var)))
    closure.Let(typ:, var:, val:, exp:) -> {
      let #(ctx, val) = do_mf(ctx, val)
      let #(ctx, exp) = do_mf(ctx, exp)
      #(ctx, Let(typ, var, val, exp))
    }
    closure.Call(typ:, fun:, arg:) -> {
      // ensure function is a simple value
      let #(ctx, fun_var, fun_var_name) = case unwrap_value(ctx, fun) {
        Ok(#(ctx, value)) -> {
          #(ctx, value, None)
        }
        _ -> {
          let #(ctx, fun_var_name) = fresh_temp(ctx)
          let fun_var = Var(fun.typ, fun_var_name)
          #(ctx, fun_var, Some(fun_var_name))
        }
      }
      let #(ctx, function) = do_mf(ctx, fun)

      // create temp variables for complex arguments
      let #(ctx, args_var_name) =
        list.fold(arg, #(ctx, []), fn(acc, arg) {
          let #(ctx, args) = acc
          let #(ctx, name) = case unwrap_value(ctx, arg) {
            Ok(#(ctx, value)) -> #(ctx, Error(value))
            _ -> {
              let #(ctx, name) = fresh_temp(ctx)
              #(ctx, Ok(name))
            }
          }
          #(ctx, [name, ..args])
        })
      let args_var_name = list.reverse(args_var_name)
      let args = list.zip(arg, args_var_name)

      // create the application with simple values only
      let args_var =
        list.map(args, fn(t) {
          let #(arg, name) = t
          case name {
            Ok(name) -> Var(arg.typ, name)
            Error(value) -> value
          }
        })
      let call = Call(typ, fun_var, args_var)

      // bind the arguments to the temp variables
      let #(ctx, body) =
        list.fold_right(args, #(ctx, call), fn(acc, t) {
          let #(ctx, body) = acc
          let #(arg, name) = t
          case name {
            Ok(name) -> {
              let #(ctx, arg) = do_mf(ctx, arg)
              #(ctx, Let(typ, name, arg, body))
            }
            Error(_) -> #(ctx, body)
          }
        })

      // bind the function variable if it exists
      case fun_var_name {
        Some(name) -> #(ctx, Let(typ, name, function, body))
        None -> #(ctx, body)
      }
    }
    closure.CallClosure(typ:, fun:, arg:) -> {
      // ensure function is a simple value
      let #(ctx, fun_var, fun_var_name) = case unwrap_value(ctx, fun) {
        Ok(#(ctx, value)) -> {
          #(ctx, value, None)
        }
        _ -> {
          let #(ctx, fun_var_name) = fresh_temp(ctx)
          let fun_var = Var(fun.typ, fun_var_name)
          #(ctx, fun_var, Some(fun_var_name))
        }
      }
      let #(ctx, function) = do_mf(ctx, fun)

      // create temp variables for complex arguments
      let #(ctx, args_var_name) =
        list.fold(arg, #(ctx, []), fn(acc, arg) {
          let #(ctx, args) = acc
          let #(ctx, name) = case unwrap_value(ctx, arg) {
            Ok(#(ctx, value)) -> #(ctx, Error(value))
            _ -> {
              let #(ctx, name) = fresh_temp(ctx)
              #(ctx, Ok(name))
            }
          }
          #(ctx, [name, ..args])
        })
      let args_var_name = list.reverse(args_var_name)
      let args = list.zip(arg, args_var_name)

      // create the application with simple values only
      let args_var =
        list.map(args, fn(t) {
          let #(arg, name) = t
          case name {
            Ok(name) -> Var(arg.typ, name)
            Error(value) -> value
          }
        })
      let call = CallClosure(typ, fun_var, args_var)

      // bind the arguments to the temp variables
      let #(ctx, body) =
        list.fold_right(args, #(ctx, call), fn(acc, t) {
          let #(ctx, body) = acc
          let #(arg, name) = t
          case name {
            Ok(name) -> {
              let #(ctx, arg) = do_mf(ctx, arg)
              #(ctx, Let(typ, name, arg, body))
            }
            Error(_) -> #(ctx, body)
          }
        })

      // bind the function variable if it exists
      case fun_var_name {
        Some(name) -> #(ctx, Let(typ, name, function, body))
        None -> #(ctx, body)
      }
    }
    closure.Op(typ:, op:, arg:) -> {
      // create temp variables for complex arguments
      let #(ctx, args_var_name) =
        list.fold(arg, #(ctx, []), fn(acc, arg) {
          let #(ctx, args) = acc
          let #(ctx, name) = case unwrap_value(ctx, arg) {
            Ok(#(ctx, value)) -> #(ctx, Error(value))
            _ -> {
              let #(ctx, name) = fresh_temp(ctx)
              #(ctx, Ok(name))
            }
          }
          #(ctx, [name, ..args])
        })
      let args_var_name = list.reverse(args_var_name)
      let args = list.zip(arg, args_var_name)

      // create the application with simple values only
      let args_var =
        list.map(args, fn(t) {
          let #(arg, name) = t
          case name {
            Ok(name) -> Var(arg.typ, name)
            Error(value) -> value
          }
        })
      let call = Value(typ, Op(typ, op, args_var))

      // bind the arguments to the temp variables
      list.fold_right(args, #(ctx, call), fn(acc, t) {
        let #(ctx, body) = acc
        let #(arg, name) = t
        case name {
          Ok(name) -> {
            let #(ctx, arg) = do_mf(ctx, arg)
            #(ctx, Let(typ, name, arg, body))
          }
          Error(_) -> #(ctx, body)
        }
      })
    }
    closure.If(typ:, cond:, then_exp:, else_exp:) -> {
      case unwrap_value(ctx, cond) {
        // some sneaky optimisations
        Ok(#(ctx, Literal(_, core.Bool("True")))) -> do_mf(ctx, then_exp)
        Ok(#(ctx, Literal(_, core.Bool("False")))) -> do_mf(ctx, else_exp)
        // condition is already a value
        Ok(#(ctx, condition)) -> {
          let #(ctx, then_branch) = do_mf(ctx, then_exp)
          let #(ctx, else_branch) = do_mf(ctx, else_exp)
          #(ctx, If(typ, condition, then_branch, else_branch))
        }
        // otherwise bind condition to a variable to make it a value
        _ -> {
          let #(ctx, condition_name) = fresh_temp(ctx)
          let condition_var = Var(cond.typ, condition_name)
          let #(ctx, condition) = do_mf(ctx, cond)
          let #(ctx, then_branch) = do_mf(ctx, then_exp)
          let #(ctx, else_branch) = do_mf(ctx, else_exp)
          let body = If(typ, condition_var, then_branch, else_branch)
          #(ctx, Let(typ, condition_name, condition, body))
        }
      }
    }
    closure.Panic(typ, arg) -> {
      case unwrap_value(ctx, arg) {
        Ok(#(ctx, value)) -> #(ctx, Panic(typ, value))
        _ -> {
          let #(ctx, arg_name) = fresh_temp(ctx)
          let #(ctx, arg) = do_mf(ctx, arg)
          let arg_var = Var(arg.typ, arg_name)
          let body = Panic(typ, arg_var)
          #(ctx, Let(typ, arg_name, arg, body))
        }
      }
    }
  }
}
