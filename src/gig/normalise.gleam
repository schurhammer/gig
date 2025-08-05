import gig/closure
import gig/core.{type LiteralKind, type Type}
import gleam/int
import gleam/io
import gleam/list
import pprint

pub type Value {
  Literal(typ: Type, val: LiteralKind)
  Variable(typ: Type, var: String)
}

pub type Term {
  Value(typ: Type, value: Value)
  Call(typ: Type, fun: Value, arg: List(Value))
  CallClosure(typ: Type, fun: Value, arg: List(Value))
  Let(typ: Type, var: String, val: Term, exp: Term)
  If(typ: Type, cond: Value, then_exp: Term, else_exp: Term)
  Panic(typ: Type, e: Value)
}

fn do_exp(uid: Int, exp: closure.Exp) -> #(Int, Term) {
  case exp {
    closure.Literal(typ, val) -> #(uid, Value(typ, Literal(typ, val)))
    closure.Var(typ, val) -> #(uid, Value(typ, Variable(typ, val)))
    closure.Call(typ, fun, args) -> {
      // normalize the function expression
      let #(uid, fun_exp) = do_exp(uid, fun)

      let #(uid, call) = case fun_exp {
        Value(_, fun_val) -> #(uid, fn(arg_refs) {
          Call(typ, fun_val, arg_refs)
        })
        _ -> {
          let fun_name = "F" <> int.to_string(uid)
          let uid = uid + 1
          let fun_val = Variable(fun_exp.typ, fun_name)
          #(uid, fn(arg_refs) {
            Let(fun_exp.typ, fun_name, fun_exp, Call(typ, fun_val, arg_refs))
          })
        }
      }

      // assign variable names to each argument (recursively)
      let #(uid, args) =
        list.fold(args, #(uid, []), fn(acc, arg) {
          let #(uid, args) = acc

          let name = "A" <> int.to_string(uid)
          let uid = uid + 1

          let #(uid, arg) = do_exp(uid, arg)
          #(uid, [#(name, arg), ..args])
        })

      // get a list of references to those variables
      let arg_refs =
        args
        |> list.map(fn(arg) {
          // values are inlined
          case arg.1 {
            Value(_, Literal(typ, x)) -> Literal(typ, x)
            Value(_, Variable(typ, x)) -> Variable(typ, x)
            _ -> Variable({ arg.1 }.typ, arg.0)
          }
        })
        |> list.reverse()

      // build the call using the variables instead of expressions arguments
      let call = call(arg_refs)

      // add the let bindings for each argument variable
      let call =
        list.fold(args, call, fn(acc, arg) {
          let #(name, val) = arg
          // values are inlined
          case val {
            Value(_, _) -> acc
            _ -> Let(typ, name, val, acc)
          }
        })
      #(uid, call)
    }
    closure.CallClosure(typ, fun, args) -> {
      // normalize the function expression
      let #(uid, fun_exp) = do_exp(uid, fun)

      let #(uid, call) = case fun_exp {
        Value(_, fun_val) -> #(uid, fn(arg_refs) {
          CallClosure(typ, fun_val, arg_refs)
        })
        _ -> {
          let fun_name = "F" <> int.to_string(uid)
          let uid = uid + 1
          let fun_val = Variable(fun_exp.typ, fun_name)
          #(uid, fn(arg_refs) {
            Let(
              fun_exp.typ,
              fun_name,
              fun_exp,
              CallClosure(typ, fun_val, arg_refs),
            )
          })
        }
      }

      // assign variable names to each argument (recursively)
      let #(uid, args) =
        list.fold(args, #(uid, []), fn(acc, arg) {
          let #(uid, args) = acc

          let name = "A" <> int.to_string(uid)
          let uid = uid + 1

          let #(uid, arg) = do_exp(uid, arg)
          #(uid, [#(name, arg), ..args])
        })

      // get a list of references to those variables
      let arg_refs =
        args
        |> list.map(fn(arg) { Variable({ arg.1 }.typ, arg.0) })
        |> list.reverse()

      // build the call using the variables instead of expressions arguments
      let call = call(arg_refs)

      // add the let bindings for each argument variable
      let call =
        list.fold(args, call, fn(acc, arg) {
          let #(name, val) = arg
          Let(typ, name, val, acc)
        })
      #(uid, call)
    }
    closure.Let(typ, name, val, body) -> {
      let #(uid, val) = do_exp(uid, val)
      let #(uid, body) = do_exp(uid, body)
      #(uid, Let(typ, name, val, body))
    }
    closure.If(typ, cond, then_exp, else_exp) -> {
      let #(uid, cond) = do_exp(uid, cond)
      let #(uid, then_exp) = do_exp(uid, then_exp)
      let #(uid, else_exp) = do_exp(uid, else_exp)

      // ensure condition is a value
      case cond {
        Value(_, cond) -> {
          let exp = If(typ, cond, then_exp, else_exp)
          #(uid, exp)
        }
        cond -> {
          let cond_name = "C" <> int.to_string(uid)
          let uid = uid + 1
          let exp = If(typ, Variable(cond.typ, cond_name), then_exp, else_exp)
          let exp = Let(cond.typ, cond_name, cond, exp)
          #(uid, exp)
        }
      }
    }
    closure.Panic(typ, val) -> {
      let val_name = "P" <> int.to_string(uid)
      let uid = uid + 1

      let #(uid, val) = do_exp(uid, val)

      let exp = Panic(typ, Variable(typ, val_name))
      let exp = Let(exp.typ, val_name, val, exp)

      #(uid, exp)
    }
  }
}

pub fn normalise_exp(exp: closure.Exp, uid: Int) -> Term {
  let #(uid, term) = do_exp(uid, exp)
  term
}
