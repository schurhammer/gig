import call_graph
import env.{type Env}
import graph

import glance as g

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}

pub type VarName =
  String

pub type TypeVarRef {
  TypeVarRef(id: Int)
}

pub type Function {
  Function(name: VarName, typ: Poly, params: List(String), body: Exp)
  Constructor(name: VarName, typ: Poly, variant: Variant, custom: CustomType)
  Getter(
    name: VarName,
    typ: Poly,
    variant: Variant,
    field: Field,
    custom: CustomType,
  )
}

pub type CustomType {
  CustomType(
    name: String,
    params: List(String),
    variants: List(Variant),
    typ: Poly,
  )
}

pub type Variant {
  Variant(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Type)
}

// TODO What if instead of
// Var(n)
// we had something like
// Var(Type(n) | Fun(n) | Local(n))

pub type Exp {
  Int(typ: Type, val: String)
  Var(typ: Type, var: VarName)
  Call(typ: Type, fun: Exp, args: List(Exp))
  Fn(typ: Type, var: List(VarName), exp: Exp)
  Let(typ: Type, var: VarName, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type Poly {
  Poly(vars: List(Int), typ: Type)
}

pub type Type {
  TypeVar(var: TypeVarRef)
  TypeApp(typ: String, args: List(Type))
  TypeFun(ret: Type, args: List(Type))
}

pub type TypeVar {
  Bound(Type)
  Unbound(id: Int, level: Int)
}

pub type TypeEnv =
  Env(VarName, CustomType)

pub type ValueEnv =
  Env(VarName, Poly)

pub type TypeVarEnv =
  Env(TypeVarRef, TypeVar)

pub type Context {
  Context(
    type_vars: TypeVarEnv,
    types: List(CustomType),
    functions: List(Function),
    type_uid: Int,
    // TODO I think we technically only need two levels
    // "top level" or "local level"
    // since we only generalize top level functions (citation needed)
    level: Int,
  )
}

const int = TypeApp("Int", [])

const bool = TypeApp("Bool", [])

const bool_binop = TypeFun(bool, [bool, bool])

const int_binop = TypeFun(int, [int, int])

const panic_ast = g.Call(g.Variable("panic"), [])

fn get_id(a: Type) -> Int {
  let assert TypeVar(a) = a
  a.id
}

fn prelude(c: Context) -> #(Context, ValueEnv) {
  let n = env.new()

  let #(c, a) = new_type_var_ref(c)
  let n = env.put(n, "panic", Poly([get_id(a)], TypeFun(a, [])))
  let n = env.put(n, "equal", Poly([get_id(a)], TypeFun(bool, [a, a])))

  let n = env.put(n, "add_int", Poly([], int_binop))
  let n = env.put(n, "sub_int", Poly([], int_binop))
  let n = env.put(n, "mul_int", Poly([], int_binop))
  let n = env.put(n, "div_int", Poly([], int_binop))

  let n = env.put(n, "and_bool", Poly([], bool_binop))

  #(c, n)
}

pub fn infer_module(mod: g.Module) {
  let c =
    Context(
      type_vars: env.new(),
      types: [],
      functions: [],
      type_uid: 0,
      level: 0,
    )

  let #(c, n) = prelude(c)

  // types
  let #(c, n) =
    list.fold(mod.custom_types, #(c, n), fn(acc, def) {
      let #(c, n) = acc
      let #(c, custom) = infer_custom_type(c, def.definition)

      io.debug(custom)

      let #(c, n) =
        list.fold(custom.variants, #(c, n), fn(acc, v) {
          let #(c, n) = acc

          // register constructor function
          let vars = custom.typ.vars
          let custom_typ = custom.typ.typ
          let args = list.map(v.fields, fn(f) { f.typ })
          let typ = Poly(vars, TypeFun(custom_typ, args))

          // put constructor into env and module definition
          let n = env.put(n, v.name, typ)
          let funs = [Constructor(v.name, typ, v, custom), ..c.functions]
          let c = Context(..c, functions: funs)

          // register getters for each field
          let #(c, n) =
            list.fold(v.fields, #(c, n), fn(acc, f) {
              let #(c, n) = acc
              let getter_name = v.name <> "_" <> f.name
              let typ = Poly(vars, TypeFun(f.typ, [custom_typ]))

              // put getter into env and module definition
              let n = env.put(n, getter_name, typ)
              let funs = [Getter(getter_name, typ, v, f, custom), ..c.functions]
              let c = Context(..c, functions: funs)
              #(c, n)
            })

          #(c, n)
        })

      let c = Context(..c, types: [custom, ..c.types])
      env.debug(n)
      #(c, n)
    })

  // functions

  let rec_groups =
    call_graph.create(mod)
    |> graph.strongly_connected_components()

  let #(c, n) =
    list.fold(rec_groups, #(c, n), fn(acc, group) {
      let #(c, n) = acc

      // enter let level
      let c = enter_level(c)

      // put the functions into env with placeholder types
      let #(c, n) =
        list.fold(group, #(c, n), fn(acc, name) {
          let #(c, n) = acc
          let #(c, t) = new_type_var_ref(c)
          let n = env.put(n, name, Poly([], t))
          #(c, n)
        })

      // infer each function
      let #(c, n, group) =
        list.fold(group, #(c, n, []), fn(acc, fun_name) {
          let #(c, n, group) = acc

          // find the function definition by name
          let assert Ok(def) =
            list.find(mod.functions, fn(x) {
              let fun = x.definition
              fun.name == fun_name
            })

          // infer the function
          let #(c, fun) = infer_function(c, n, def.definition)

          #(c, n, [fun, ..group])
        })

      // exit let level
      let c = exit_level(c)

      // generalize functions
      let #(n, group) =
        list.fold(group, #(n, []), fn(acc, fun) {
          let #(n, group) = acc
          // TODO refactor to remove the 4 tuple for Function
          let #(name, params, body, typ) = fun
          let gen = generalise(c, typ)
          let fun = Function(name, gen, params, body)
          let n = env.put(n, name, gen)
          #(n, [fun, ..group])
        })

      let c = Context(..c, functions: list.append(group, c.functions))

      #(c, n)
    })

  unshadow_context(c)
}

pub fn get_type_var(c: Context, var: TypeVarRef) {
  let assert Ok(x) = env.get(c.type_vars, var)
  x
}

fn set_type_var(c: Context, var: TypeVarRef, bind: TypeVar) {
  Context(..c, type_vars: env.put(c.type_vars, var, bind))
}

fn new_type_var_ref(c: Context) -> #(Context, Type) {
  let ref = TypeVarRef(c.type_uid)
  let type_vars = env.put(c.type_vars, ref, Unbound(c.type_uid, c.level))
  #(Context(..c, type_vars: type_vars, type_uid: c.type_uid + 1), TypeVar(ref))
}

fn infer_custom_type(c: Context, ct: g.CustomType) {
  // create a type variable for each parameter
  // these will be used when a field references a type parameter
  let #(c, param_types) =
    list.fold(ct.parameters, #(c, []), fn(acc, p) {
      // TODO I think we technically don't need to create refs here, we can just use 1,2,3
      let #(c, l) = acc
      let #(c, t) = new_type_var_ref(c)
      let assert TypeVar(var) = t
      #(c, [#(p, t, var.id), ..l])
    })
  let param_types = list.reverse(param_types)

  // create an env for param types
  let param_env =
    list.fold(param_types, env.new(), fn(n, p) { env.put(n, p.0, p.1) })

  // process each variant
  let #(c, variants) =
    list.fold(ct.variants, #(c, []), fn(acc, variant) {
      let #(c, l) = acc
      let #(c, v) = infer_variant(c, param_env, variant)
      #(c, [v, ..l])
    })
  let variants = list.reverse(variants)

  // create a poly type
  let type_args = list.map(param_types, fn(x) { x.1 })
  let type_params = list.map(param_types, fn(x) { x.2 })
  let typ = Poly(type_params, TypeApp(ct.name, type_args))

  #(c, CustomType(ct.name, ct.parameters, variants, typ))
}

fn infer_variant(c: Context, n: Env(String, Type), variant: g.Variant) {
  let #(c, fields) =
    list.index_fold(variant.fields, #(c, []), fn(acc, field, i) {
      let #(c, l) = acc
      let #(c, t) = infer_type(c, n, field.item)
      let name = case field.label {
        Some(x) -> x
        None -> "field" <> int.to_string(i)
      }
      #(c, [Field(name, t), ..l])
    })
  let fields = list.reverse(fields)

  #(c, Variant(variant.name, fields))
}

fn infer_type(c: Context, n: Env(String, Type), typ: g.Type) {
  case typ {
    g.NamedType(name, module, params) -> {
      let #(c, params) =
        list.fold(params, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = infer_type(c, n, p)
          #(c, [p, ..l])
        })
      let params = list.reverse(params)
      #(c, TypeApp(name, params))
    }
    g.TupleType(elements) -> todo
    g.FunctionType(parameters, return) -> todo
    g.VariableType(name) -> {
      let assert Ok(t) = env.get(n, name)
      #(c, t)
    }
    g.HoleType(name) -> todo
  }
}

fn infer_function(
  c: Context,
  n: ValueEnv,
  fun: g.Function,
) -> #(Context, #(String, List(String), Exp, Type)) {
  // get list of param names
  let params =
    list.map(fun.parameters, fn(param) {
      case param.name {
        g.Named(s) -> s
        g.Discarded(s) -> s
      }
    })

  // create type vars for parameters
  let #(c, n, param_types) =
    list.fold(params, #(c, n, []), fn(acc, param) {
      let #(c, n, param_types) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = env.put(n, param, Poly([], typ))
      #(c, n, [typ, ..param_types])
    })
  let param_types = list.reverse(param_types)

  // infer body
  let #(c, body) = infer_body(c, n, fun.body)

  // compute function type
  let typ = TypeFun(body.typ, param_types)
  let assert Ok(Poly([], typ0)) = env.get(n, fun.name)
  let c = unify(c, typ, typ0)

  #(c, #(fun.name, params, body, typ))
}

fn enter_level(c: Context) -> Context {
  Context(..c, level: c.level + 1)
}

fn exit_level(c: Context) -> Context {
  Context(..c, level: c.level - 1)
}

fn infer_body(
  c: Context,
  n: ValueEnv,
  body: List(g.Statement),
) -> #(Context, Exp) {
  case body {
    [] -> panic as "empty body"
    [x] ->
      case x {
        // TODO do we need to enter_level for these?
        g.Use(..) -> todo as "use statement"
        g.Assignment(value: value, ..) -> infer_expression(c, n, value)
        g.Expression(value) -> infer_expression(c, n, value)
      }
    [x, ..xs] ->
      case x {
        g.Use(..) -> todo as "use statement"
        g.Assignment(value: value, pattern: pattern, ..) -> {
          let c = enter_level(c)
          let #(c, value) = infer_expression(c, n, value)
          let c = exit_level(c)

          let subject_name = "S"
          let n = env.put(n, subject_name, Poly([], value.typ))
          let #(c, subject) = infer_expression(c, n, g.Variable(subject_name))
          let #(c, bindings) = infer_bind_pattern(c, n, pattern, subject)

          // add bindings to environment
          let n =
            list.fold(bindings, n, fn(n, binding) {
              let #(name, value) = binding
              env.put(n, name, Poly([], value.typ))
            })

          // infer the rest of the body
          let #(c, body) = infer_body(c, n, xs)

          // generate let bindings followed by rest of body
          let body =
            list.fold(bindings, body, fn(body, binding) {
              let #(name, value) = binding
              Let(body.typ, name, value, body)
            })

          // add binding for the subject
          let body = Let(body.typ, subject_name, value, body)

          #(c, body)
        }
        g.Expression(value) -> {
          let c = enter_level(c)
          let #(c, value) = infer_expression(c, n, value)
          let c = exit_level(c)
          let #(c, in) = infer_body(c, n, xs)
          #(c, Let(in.typ, "_", value, in))
        }
      }
  }
}

fn infer_bind_pattern(
  c: Context,
  n: ValueEnv,
  pattern: g.Pattern,
  subject: Exp,
) -> #(Context, List(#(String, Exp))) {
  // TODO enter_level here? or do we not need to because gleam doesn't do let-poly
  // I think its done in infer_body already
  case pattern {
    g.PatternInt(_) -> #(c, [])
    g.PatternVariable(x) -> {
      // let n = env.put(n, x, Poly([], subject.typ))
      // let #(c, body) = infer_body(c, n, body)
      // Let(body.typ, x, subject, body)
      #(c, [#(x, subject)])
    }
    _ -> {
      io.debug(pattern)
      todo as "not implemented"
    }
  }
}

fn infer_check_pattern(
  c: Context,
  n: ValueEnv,
  pattern: g.Pattern,
  subject: g.Expression,
) -> #(Context, Exp) {
  case pattern {
    g.PatternInt(value) -> {
      let args = [g.Field(None, g.Int(value)), g.Field(None, subject)]
      infer_expression(c, n, g.Call(g.Variable("equal"), args))
    }
    g.PatternVariable(_) -> #(c, Var(bool, "True"))
    // g.PatternConstructor(_, constructor, arguments, _) -> {
    //   todo
    // }
    _ -> {
      io.debug(pattern)
      io.debug(subject)
      todo
    }
  }
}

fn infer_expression(
  c: Context,
  n: ValueEnv,
  exp: g.Expression,
) -> #(Context, Exp) {
  io.debug(exp)
  case exp {
    g.Int(s) -> #(c, Int(int, s))
    g.Variable(s) -> {
      // instantiate the poly type into a mono type
      let poly = case env.get(n, s) {
        Ok(poly) -> poly
        Error(_) -> {
          io.debug(s)
          panic as "variable not in env"
        }
      }
      let #(c, typ) = instantiate(c, poly)
      #(c, Var(typ, s))
    }
    g.Call(fun, args) -> {
      // infer the type of the function
      let #(c, fun) = infer_expression(c, n, fun)

      // infer type of each arg
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, l) = acc
          let #(c, arg) = infer_expression(c, n, arg.item)
          #(c, [arg, ..l])
        })
      let args = list.reverse(args)

      // new var for the return type
      let #(c, ret) = new_type_var_ref(c)
      let arg_types = list.map(args, fn(x) { x.typ })

      // unify the actual function type with the types of args
      let c = unify(c, fun.typ, TypeFun(ret, arg_types))

      #(c, Call(ret, fun, args))
    }
    g.Fn(params, _, body) -> {
      // get list of param names
      let params =
        list.map(params, fn(param) {
          case param.name {
            g.Named(s) -> s
            g.Discarded(s) -> s
          }
        })

      // create type vars for parameters
      let #(c, n, param_types) =
        list.fold(params, #(c, n, []), fn(acc, param) {
          let #(c, n, param_types) = acc
          let #(c, typ) = new_type_var_ref(c)
          let n = env.put(n, param, Poly([], typ))
          #(c, n, [typ, ..param_types])
        })
      let param_types = list.reverse(param_types)

      // infer body
      let #(c, body) = infer_body(c, n, body)

      // compute function type
      let typ = TypeFun(body.typ, param_types)

      #(c, Fn(typ, params, body))
    }
    g.BinaryOperator(name, left, right) -> {
      let name = case name {
        g.AddInt -> "add_int"
        g.SubInt -> "sub_int"
        g.MultInt -> "mul_int"
        g.DivInt -> "div_int"
        _ -> {
          io.debug(name)
          todo as "not implemented"
        }
      }
      let args = [g.Field(None, left), g.Field(None, right)]
      let fun = g.Call(g.Variable(name), args)
      infer_expression(c, n, fun)
    }
    g.Case(subjects, clauses) -> {
      // unwrap alternative case patterns into multiple clauses
      // we do this because they bind variables differently
      let clauses =
        list.flat_map(clauses, fn(clause: g.Clause) {
          list.map(clause.patterns, fn(patterns) { #(patterns, clause.body) })
        })

      // need to reverse to make the last one the innermost after the fold
      let clauses = list.reverse(clauses)

      // the innermost branch will panic due to unmatched value
      let #(c, panic_exp) = infer_expression(c, n, panic_ast)

      // create let binding for subjects
      let #(c, n, subjects) =
        list.index_fold(subjects, #(c, n, []), fn(acc, subject, i) {
          let #(c, n, l) = acc
          let c = enter_level(c)
          let #(c, value) = infer_expression(c, n, subject)
          let c = exit_level(c)
          let subject_name = "S" <> int.to_string(i)
          let n = env.put(n, subject_name, Poly([], value.typ))
          #(c, n, [#(subject_name, value), ..l])
        })

      // process each clause in order
      let #(c, e) =
        list.fold(clauses, #(c, panic_exp), fn(acc, clause) {
          // grab the and_bool function
          let and_bool = g.Variable("and_bool")
          let #(c, and_bool) = infer_expression(c, n, and_bool)

          let #(c, else_exp) = acc
          let #(patterns, body) = clause

          // check each subject
          // TODO is defaulting to true ok?
          let #(c, cond_exp) =
            list.index_fold(
              patterns,
              #(c, Var(bool, "True")),
              fn(acc, pattern, i) {
                let #(c, last_e) = acc

                // get the subject
                let subject = g.Variable("S" <> int.to_string(i))
                // let #(c, subject) = infer_expression(c, n, subject)

                // check that the subject matches the pattern
                let #(c, e) = infer_check_pattern(c, n, pattern, subject)

                // TODO do we need to check that e is Bool?
                #(c, Call(bool, and_bool, [e, last_e]))
              },
            )

          // find bindings for all subjects
          let #(c, bindings) =
            list.index_fold(patterns, #(c, []), fn(acc, pattern, i) {
              let #(c, l) = acc
              let subject = g.Variable("S" <> int.to_string(i))
              let #(c, subject) = infer_expression(c, n, subject)
              let #(c, bindings) = infer_bind_pattern(c, n, pattern, subject)
              // TODO use efficient combine function instead of append?
              #(c, list.append(bindings, l))
            })

          // apply binding to env
          let n =
            list.fold(bindings, n, fn(n, binding) {
              let #(name, val) = binding
              env.put(n, name, Poly([], val.typ))
            })

          // infer the body
          let #(c, body) = infer_expression(c, n, body)

          // add let expressions to define the bindings
          let then_exp =
            list.fold(bindings, body, fn(body, binding) {
              let #(name, value) = binding
              Let(body.typ, name, value, body)
            })

          // unify to make sure the types match up
          let c = unify(c, cond_exp.typ, bool)
          let c = unify(c, then_exp.typ, else_exp.typ)

          // create an if expression
          #(c, If(then_exp.typ, cond_exp, then_exp, else_exp))
        })

      // add let expressions to define the subjects
      let e = list.fold(subjects, e, fn(e, s) { Let(e.typ, s.0, s.1, e) })

      #(c, e)
    }
    g.FieldAccess(value, field) -> {
      // TODO probably need to do more work to resolve this type properly
      let #(c, exp) = infer_expression(c, n, value)
      let assert TypeApp(type_name, _) = exp.typ

      // accessor only works with exactly one variant
      let assert Ok(custom) = list.find(c.types, fn(x) { x.name == type_name })
      let assert [variant] = custom.variants

      let getter_name = variant.name <> "_" <> field

      // TODO we're compiling 'value' twice, try to fix that (maybe with a temp variable?)
      let #(c, exp) =
        infer_expression(
          c,
          n,
          g.Call(g.Variable(getter_name), [g.Field(None, value)]),
        )

      io.debug(exp)
      #(c, exp)
    }
    exp -> {
      io.debug(exp)
      todo as "not implemented"
    }
  }
}

fn is_bound(c: Context, a: Type) -> Bool {
  case a {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(a) -> True
        _ -> False
      }
    _ -> False
  }
}

fn occurs(c: Context, id: Int, level: Int, in: Type) -> #(Context, Bool) {
  case in {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(t) -> occurs(c, id, level, t)
        Unbound(i, l) -> {
          let min = int.min(l, level)
          let c = set_type_var(c, ref, Unbound(i, min))
          #(c, id == i)
        }
      }
    TypeApp(_, args) ->
      list.fold(args, #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, level, arg)
        #(c, b || b1)
      })
    TypeFun(fun, args) ->
      list.fold([fun, ..args], #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, level, arg)
        #(c, b || b1)
      })
  }
}

fn unify(c: Context, a: Type, b: Type) -> Context {
  // TODO not sure if this is_bound check are necessary
  let a_bound = is_bound(c, a)
  let b_bound = is_bound(c, b)
  case a, b, a_bound, b_bound {
    TypeVar(ref), b, True, _ -> {
      let assert Bound(a) = get_type_var(c, ref)
      unify(c, a, b)
    }
    a, TypeVar(ref), _, True -> {
      let assert Bound(b) = get_type_var(c, ref)
      unify(c, a, b)
    }
    TypeVar(ref), b, _, _ ->
      case a == b {
        True -> c
        False -> {
          let assert Unbound(aid, alevel) = get_type_var(c, ref)
          let #(c, occurs) = occurs(c, aid, alevel, b)
          case occurs {
            True -> {
              io.debug(a)
              io.debug(b)
              panic as "recursive type"
            }
            False -> {
              set_type_var(c, ref, Bound(b))
            }
          }
        }
      }
    a, TypeVar(ref), _, _ ->
      case a == b {
        True -> c
        False -> {
          let assert Unbound(bid, blevel) = get_type_var(c, ref)
          let #(c, occurs) = occurs(c, bid, blevel, a)
          case occurs {
            True -> {
              io.debug(a)
              io.debug(b)
              panic as "recursive type"
            }
            False -> {
              set_type_var(c, ref, Bound(a))
            }
          }
        }
      }
    TypeApp(aname, _), TypeApp(bname, _), _, _ if aname != bname -> {
      io.debug(a)
      io.debug(b)
      panic as "failed to unify types"
    }
    TypeApp(_, aargs), TypeApp(_, bargs), _, _ -> {
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          io.debug(a)
          io.debug(b)
          panic as "incorrect number of type arguments"
        }
      }
    }
    TypeFun(aret, aargs), TypeFun(bret, bargs), _, _ -> {
      let c = unify(c, aret, bret)
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          io.debug(a)
          io.debug(b)
          panic as "incorrect number of function arguments"
        }
      }
    }
    _, _, _, _ -> {
      io.debug(a)
      io.debug(b)
      panic as "failed to unify types"
    }
  }
}

fn instantiate(c: Context, poly: Poly) -> #(Context, Type) {
  do_instantiate(c, env.new(), poly)
}

fn do_instantiate(c: Context, n: Env(Int, Type), poly: Poly) -> #(Context, Type) {
  let #(c, n) =
    list.fold(poly.vars, #(c, n), fn(acc, var) {
      let #(c, n) = acc
      let #(c, new_var) = new_type_var_ref(c)
      let n = env.put(n, var, new_var)
      #(c, n)
    })
  #(c, do_instantiate_type(c, n, poly.typ))
}

fn do_instantiate_type(c: Context, n: Env(Int, Type), typ: Type) -> Type {
  case typ {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> do_instantiate_type(c, n, x)
        Unbound(x, l) ->
          case env.get(n, x) {
            Ok(r) -> r
            Error(_) -> typ
          }
      }
    TypeApp(name, args) ->
      TypeApp(name, list.map(args, do_instantiate_type(c, n, _)))
    TypeFun(fun, args) ->
      TypeFun(
        do_instantiate_type(c, n, fun),
        list.map(args, do_instantiate_type(c, n, _)),
      )
  }
}

fn generalise(c: Context, t: Type) -> Poly {
  let tvs =
    list.unique(find_tvs(c, t))
    |> list.sort(int.compare)
  Poly(tvs, t)
}

fn find_tvs(c: Context, t: Type) -> List(Int) {
  case t {
    TypeVar(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> find_tvs(c, x)
        Unbound(x, l) -> {
          case l > c.level {
            True -> [x]
            False -> []
          }
        }
      }
    TypeApp(_, args) -> list.flat_map(args, find_tvs(c, _))
    TypeFun(fun, args) -> list.flat_map([fun, ..args], find_tvs(c, _))
  }
}

fn unshadow(taken: List(String), i: Int, e: Exp) {
  case e {
    Int(_, _) -> e
    Var(_, _) -> e
    Call(typ, fun, args) -> {
      let fun = unshadow(taken, i, fun)
      let args = list.map(args, unshadow(taken, i, _))
      Call(typ, fun, args)
    }
    Fn(typ, vars, exp) -> {
      let #(taken, i, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var) {
            True -> {
              let new_var = var <> "_V" <> int.to_string(i)
              let i = i + 1
              let exp = rename_var(var, new_var, exp)
              let var = new_var
              let taken = [var, ..taken]
              let exp = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
            False -> {
              let taken = [var, ..taken]
              let exp = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
          }
        })
      let exp = unshadow(taken, i, exp)
      let vars = list.reverse(vars)
      Fn(typ, vars, exp)
    }
    Let(typ, var, val, exp) ->
      case list.contains(taken, var) {
        True -> {
          let val = unshadow(taken, i, val)
          let new_var = var <> "_V" <> int.to_string(i)
          let i = i + 1
          let exp = rename_var(var, new_var, exp)
          let var = new_var
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          Let(typ, var, val, exp)
        }
        False -> {
          let val = unshadow(taken, i, val)
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          Let(typ, var, val, exp)
        }
      }
    If(typ, cond, then_exp, else_exp) -> {
      let cond = unshadow(taken, i, cond)
      let then_exp = unshadow(taken, i, then_exp)
      let else_exp = unshadow(taken, i, else_exp)
      If(typ, cond, then_exp, else_exp)
    }
  }
}

fn unshadow_context(c: Context) -> Context {
  let globals = list.map(c.functions, fn(fun) { fun.name })
  let functions =
    list.map(c.functions, fn(fun) {
      case fun {
        Function(name, typ, params, body) ->
          Function(name, typ, params, unshadow(globals, 1, body))
        Constructor(..) -> fun
        Getter(..) -> fun
      }
    })
  Context(..c, functions: functions)
}

fn rename_var(replace: String, with: String, in: Exp) -> Exp {
  case in {
    Int(_, _) -> in
    Var(typ, var) ->
      case var == replace {
        True -> Var(typ, with)
        False -> in
      }
    Call(typ, fun, args) -> {
      let fun = rename_var(replace, with, fun)
      let args = list.map(args, rename_var(replace, with, _))
      Call(typ, fun, args)
    }
    Fn(typ, vars, exp) ->
      case list.contains(vars, replace) {
        True -> in
        False -> Fn(typ, vars, rename_var(replace, with, exp))
      }
    Let(typ, var, val, exp) ->
      case var == replace {
        True -> Let(typ, var, rename_var(replace, with, val), exp)
        False ->
          Let(
            typ,
            var,
            rename_var(replace, with, val),
            rename_var(replace, with, exp),
          )
      }
    If(typ, cond, then_exp, else_exp) ->
      If(
        typ,
        rename_var(replace, with, cond),
        rename_var(replace, with, then_exp),
        rename_var(replace, with, else_exp),
      )
  }
}
