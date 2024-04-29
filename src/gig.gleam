import glance as g
import gleam/io
import gleam/list
import gleam/option.{None, Some}

type Context =
  List(#(String, g.Type))

fn check_module(con: Context, mod: g.Module) -> Context {
  mod.functions
  |> list.fold(con, fn(con, d) { check_function(con, d.definition).0 })
}

fn check_function(con: Context, fun: g.Function) -> #(Context, g.Type) {
  let #(_, typ) =
    fun.body
    |> list.fold(#(con, g.HoleType("*")), fn(con, d) {
      check_statement(con.0, d)
    })

  // add the function to the context
  // TODO it should be a function type not just the return type
  #([#(fun.name, typ), ..con], typ)
}

fn check_statement(con: Context, sta: g.Statement) -> #(Context, g.Type) {
  case sta {
    g.Assignment(_kind, pat, ann, exp) -> {
      // find the type of the expression
      let typ = check_expression(con, exp)

      // make sure the expression matches the type annotation
      case ann {
        Some(ann) -> {
          let assert True = typ == ann
          Nil
        }
        None -> Nil
      }

      // add variables to context based on the pattern
      case pat {
        g.PatternVariable(name) -> #([#(name, typ), ..con], typ)

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
  con
  |> list.find_map(fn(x) {
    case x {
      #(n, t) if n == name -> Ok(t)
      _ -> Error(Nil)
    }
  })
}

fn check_expression(con: Context, exp: g.Expression) -> g.Type {
  case exp {
    g.Int(_) -> g.NamedType("Int", None, [])
    g.Float(_) -> g.NamedType("Float", None, [])
    g.String(_) -> g.NamedType("String", None, [])
    g.Variable(name) -> {
      let assert Ok(typ) = find_in_context(con, name)
      typ
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
      let left_typ = check_expression(con, left)
      let right_typ = check_expression(con, right)
      case name {
        g.AddInt -> {
          let assert g.NamedType("Int", None, []) = left_typ
          let assert g.NamedType("Int", None, []) = right_typ
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
  pub fn main() {
    let a: Int = 1
    let b = 2
    a + b
  }
  ",
    )

  io.debug(module)
  check_module([], module)
}
