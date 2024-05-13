import glance as g
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import types as t

type Context {
  Context(env: t.Env, uid: Int)
}

fn new_context() -> Context {
  Context([], 1)
}

fn next_type_var(con: Context) -> #(Context, t.Type) {
  #(Context(..con, uid: con.uid + 1), t.TypeVar(con.uid))
}

fn bind_env(con: Context, name: String, bind_type: t.Poly) -> Context {
  let env = [#(name, bind_type), ..con.env]

  env
  |> io.debug

  Context(..con, env: env)
}

fn check_module(con: Context, mod: g.Module) -> Context {
  mod.functions
  |> list.fold(con, fn(con, d) { check_function(con, d.definition).0 })
}

fn check_function(con: Context, fun: g.Function) -> #(Context, t.Expression) {
  let outer_env = con.env
  let con =
    fun.parameters
    |> list.fold(con, fn(con, param) {
      case param.name {
        g.Named(name) ->
          case param.type_ {
            Some(ann_type) -> bind_env(con, name, ann_type)
            None -> {
              let #(con, var_type) = next_type_var(con)
              bind_env(con, name, var_type)
            }
          }
        g.Discarded(_) -> con
      }
    })

  let #(con, ret_type_ann) = case fun.return {
    Some(ret_type) -> #(con, ret_type)
    None -> next_type_var(con)
  }

  let #(_, ret_type) =
    fun.body
    |> list.fold(#(con, ret_type_ann), fn(con, d) { check_statement(con.0, d) })

  let ret_type = unify(ret_type_ann, ret_type)

  // add the function to the context
  // TODO it should be a function type not just the return type
  #(bind_env(Context(..con, env: outer_env), fun.name, ret_type), ret_type)
}

fn check_statement(con: Context, sta: g.Statement) -> #(Context, t.Expression) {
  case sta {
    g.Assignment(_kind, pat, ann, exp) -> {
      // find the type of the expression
      let expr_type = check_expression(con, exp)

      // make sure the expression matches the type annotation
      case ann {
        Some(ann) -> {
          let assert True = expr_type == ann
          Nil
        }
        None -> Nil
      }

      // add variables to context based on the pattern
      case pat {
        g.PatternVariable(name) -> #(bind_env(con, name, expr_type), expr_type)

        _ -> todo
      }
    }
    g.Expression(exp) -> {
      #(con, check_expression(con, exp))
    }
    g.Use(..) -> todo
  }
}

fn find_in_context(con: Context, name: String) -> Result(t.Poly, Nil) {
  con.env
  |> list.find_map(fn(x) {
    case x {
      #(n, t) if n == name -> Ok(t)
      _ -> Error(Nil)
    }
  })
}

// TODO unify needs to alter the env too
fn unify(a: g.Type, b: g.Type) -> g.Type {
  case a, b {
    a, b if a == b -> a
    g.VariableType(_), b -> b
    a, g.VariableType(_) -> a
    _, _ -> {
      io.debug(#("failed to unify", a, b))
      panic
    }
  }
}

fn check_expression(con: Context, exp: g.Expression) -> t.Expression {
  case exp {
    g.Int(x) -> {
      let assert Ok(x) = int.parse(x)
      t.Int(t.TypeFun("Int", []), x)
    }
    g.String(x) -> t.String(t.TypeFun("String", []), x)
    g.Variable(name) -> {
      let assert Ok(typ) = find_in_context(con, name)
      t.Variable(t.inst(typ), name)
    }
    g.Call(fun, args) -> {
      let fun = check_expression(con, fun)
      let args = list.map(args, fn(arg) { check_expression(con, arg.item) })

      let subs = case fun.typ {
        t.TypeFun(name, params) -> {
          let assert Ok(zip) = list.strict_zip(params, args)
          let sub =
            list.fold(zip, [], fn(acc, pair) {
              let #(param_type, arg_type) = pair
              t.compose_sub(acc, t.mgu(param_type, arg_type.typ))
            })
          // t.Call(
          //   t.Function(apply_sub(sub, return_type), apply_sub(sub, param_types)),
          //   arg_types,
          // )
        }
        _ -> {
          io.debug(#("Expected a function type, but got", fun))
          panic
        }
      }
    }
    g.BinaryOperator(op, left, right) -> {
      check_expression(
        con,
        g.Call(g.Variable(bin_op_name(op)), [
          g.Field(option.None, left),
          g.Field(option.None, right),
        ]),
      )
    }
    _ -> {
      io.println_error("\n\ncontext\n---")
      io.debug(con)
      io.println_error("\nexpression\n---")
      io.debug(exp)
      io.println_error("\n")
      todo
    }
  }
}

fn bin_op_name(op: g.BinaryOperator) -> String {
  case op {
    g.AddInt -> "add_int"
    _ -> todo
  }
}

pub fn main() {
  let assert Ok(module) =
    g.module(
      "
      fn f(x, y) {
        x + 6
      }
  ",
    )
  check_module(new_context(), module)
  |> io.debug
  Nil
}
