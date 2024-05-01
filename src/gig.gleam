import glance as g
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}

type Context {
  Context(env: List(#(String, g.Type)), uid: Int)
}

fn new_context() -> Context {
  Context([], 1)
}

fn next_type_var(con: Context) -> #(Context, g.Type) {
  #(
    Context(..con, uid: con.uid + 1),
    g.VariableType("T" <> int.to_string(con.uid)),
  )
}

fn bind_env(con: Context, name: String, bind_type: g.Type) -> Context {
  let env = [#(name, bind_type), ..con.env]

  env
  |> io.debug

  Context(..con, env: env)
}

fn check_module(con: Context, mod: g.Module) -> Context {
  mod.functions
  |> list.fold(con, fn(con, d) { check_function(con, d.definition).0 })
}

fn check_function(con: Context, fun: g.Function) -> #(Context, g.Type) {
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

fn check_statement(con: Context, sta: g.Statement) -> #(Context, g.Type) {
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

fn find_in_context(con: Context, name: String) -> Result(g.Type, Nil) {
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

fn check_expression(con: Context, exp: g.Expression) -> g.Type {
  case exp {
    g.Int(_) -> g.NamedType("Int", None, [])
    g.Float(_) -> g.NamedType("Float", None, [])
    g.String(_) -> g.NamedType("String", None, [])
    g.Variable(name) -> {
      let assert Ok(var_type) = find_in_context(con, name)
      var_type
    }
    // g.NegateInt(Expression)
    // g.g.NegateBool(Expression)
    // g.Block(List(Statement))
    // g.Panic(Option(Expression))
    // g.Todo(Option(Expression))
    // g.Tuple(List(Expression))
    // g.List(elements: List(Expression), rest: Option(Expression))
    // g.Fn(
    //   arguments: List(FnParameter),
    //   return_annotation: Option(Type),
    //   body: List(Statement),
    // )
    // g.RecordUpdate(
    //   module: Option(String),
    //   constructor: String,
    //   record: Expression,
    //   fields: List(#(String, Expression)),
    // )
    // g.FieldAccess(container: Expression, label: String)
    // g.Call(function: Expression, arguments: List(Field(Expression)))
    // g.TupleIndex(tuple: Expression, index: Int)
    // g.FnCapture(
    //   label: Option(String),
    //   function: Expression,
    //   arguments_before: List(Field(Expression)),
    //   arguments_after: List(Field(Expression)),
    // )
    // g.BitString(
    //   segments: List(#(Expression, List(BitStringSegmentOption(Expression)))),
    // )
    // g.Case(subjects: List(Expression), clauses: List(Clause))
    g.BinaryOperator(name, left, right) -> {
      let left_type = check_expression(con, left)
      let right_type = check_expression(con, right)
      let #(con, ret_typee) = next_type_var(con)
      let inferred_type = g.FunctionType([left_type, right_type], ret_typee)
      case name {
        g.AddInt -> {
          let expected_type =
            g.FunctionType(
              [g.NamedType("Int", None, []), g.NamedType("Int", None, [])],
              g.NamedType("Int", None, []),
            )
          // TODO instead of the below, unify the function types (inferred vs expected) and apply the subs to env
          unify(g.NamedType("Int", None, []), left_type)
          unify(g.NamedType("Int", None, []), right_type)
          g.NamedType("Int", None, [])
        }
        _ -> todo
      }
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
