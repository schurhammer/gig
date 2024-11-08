import gig/closure
import gig/core.{type LiteralKind, type Type}
import gleam/int
import gleam/io
import gleam/list

pub type Value {
  Literal(typ: Type, val: LiteralKind)
  Variable(typ: Type, var: String)
  // TODO hack to fix if condition that rely on short circuiting &&
  Term(typ: Type, val: Term)
}

pub type Term {
  Value(typ: Type, value: Value)
  Call(typ: Type, fun: Value, arg: List(Value))
  CallClosure(typ: Type, fun: Value, arg: List(Value))
  Let(typ: Type, var: String, val: Term, exp: Term)
  If(typ: Type, cond: Value, then_exp: Term, else_exp: Term)
  Panic(typ: Type, e: Value)
}

fn exp_to_val(exp: closure.Exp) {
  case exp {
    closure.Literal(typ, val) -> Literal(typ, val)
    closure.Var(typ, val) -> Variable(typ, val)
    _ -> panic
  }
}

fn cond_to_term(exp: closure.Exp) -> Value {
  case exp {
    closure.Literal(typ, val) -> Literal(typ, val)
    closure.Var(typ, val) -> Variable(typ, val)
    closure.Call(typ, fun, args) -> {
      let fun = cond_to_term(fun)
      let args = list.map(args, cond_to_term)
      Term(typ, Call(typ, fun, args))
    }
    closure.CallClosure(typ, fun, args) -> {
      let fun = cond_to_term(fun)
      let args = list.map(args, cond_to_term)
      Term(typ, Call(typ, fun, args))
    }
    _ -> panic
  }
}

fn do_exp(uid: Int, exp: closure.Exp) -> #(Int, Term) {
  case exp {
    closure.Literal(typ, val) -> #(uid, Value(typ, Literal(typ, val)))
    closure.Var(typ, val) -> #(uid, Value(typ, Variable(typ, val)))
    closure.Call(typ, fun, args) -> {
      // expect fun to be a value already
      let fun = exp_to_val(fun)

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
      let call = Call(typ, fun, arg_refs)

      // add the let bindings for each argument variable
      let call =
        list.fold(args, call, fn(acc, arg) {
          let #(name, val) = arg
          Let(typ, name, val, acc)
        })
      #(uid, call)
    }
    closure.CallClosure(typ, fun, args) -> {
      // expect fun to be a value already
      let fun = exp_to_val(fun)

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
      let call = CallClosure(typ, fun, arg_refs)

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
      // TODO cond currently relies on short circuiting expressions
      // which means we can't split it up like normal code
      let cond = cond_to_term(cond)

      let cond_name = "C" <> int.to_string(uid)
      let uid = uid + 1

      let #(uid, then_exp) = do_exp(uid, then_exp)
      let #(uid, else_exp) = do_exp(uid, else_exp)

      // use the cond variable instead of the expression
      let exp = If(typ, Variable(cond.typ, cond_name), then_exp, else_exp)
      let exp = Let(cond.typ, cond_name, Value(cond.typ, cond), exp)

      #(uid, exp)
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