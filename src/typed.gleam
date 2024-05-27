import gleam/dict
import gleam/list
import gleam/result
import graph
import unique_integer

import core.{type Type, type TypeDef, TypeApp, TypeDef, TypeFun, TypeVar} as c

pub type VarName =
  String

pub type TypeVarName =
  Int

pub type Env =
  dict.Dict(VarName, Poly)

pub type Sub =
  dict.Dict(TypeVarName, Type)

pub type Module {
  Module(types: List(TypeDef), functions: List(Function))
}

pub type Function {
  Function(name: VarName, params: List(String), body: Exp, typ: Poly)
}

pub type Exp {
  Int(typ: Type, val: Int)
  Var(typ: Type, var: VarName)
  App(typ: Type, fun: Exp, arg: List(Exp))
  Abs(typ: Type, var: List(VarName), exp: Exp)
  Let(typ: Type, var: VarName, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type Poly {
  Mono(typ: Type)
  Poly(var: TypeVarName, typ: Poly)
}

pub fn apply_sub_texpr(sub: Sub, texp: Exp) -> Exp {
  case texp {
    Int(_, _) -> texp
    Var(typ, var) -> Var(apply_sub(sub, typ), var)
    App(typ, fun, arg) ->
      App(
        apply_sub(sub, typ),
        apply_sub_texpr(sub, fun),
        list.map(arg, apply_sub_texpr(sub, _)),
      )
    Abs(typ, var, exp) ->
      Abs(apply_sub(sub, typ), var, apply_sub_texpr(sub, exp))
    Let(typ, var, val, exp) ->
      Let(
        apply_sub(sub, typ),
        var,
        apply_sub_texpr(sub, val),
        apply_sub_texpr(sub, exp),
      )
    If(typ, cond, then_exp, else_exp) ->
      If(
        apply_sub(sub, typ),
        apply_sub_texpr(sub, cond),
        apply_sub_texpr(sub, then_exp),
        apply_sub_texpr(sub, else_exp),
      )
  }
}

pub fn gen(env: Env, typ: Type) -> Poly {
  let vars =
    ftv_typing(env, Mono(typ))
    |> list.unique()
  list.fold(vars, Mono(typ), fn(acc, var) { Poly(var, acc) })
}

fn inst(sub: Sub, poly: Poly) -> Type {
  case poly {
    Mono(typ) -> apply_sub(sub, typ)
    Poly(var, poly) -> {
      let new_var = new_type_var()
      let sub = dict.insert(sub, var, TypeVar(new_var))
      inst(sub, poly)
    }
  }
}

pub fn w(env: Env, exp: c.Exp) -> Result(#(Exp, Sub), String) {
  case exp {
    c.Int(val) -> Ok(#(Int(TypeApp("Int", []), val), dict.new()))
    c.Var(var) ->
      case dict.get(env, var) {
        Ok(poly) -> {
          // Instantiate the polymorphic type to get a monomorphic type
          let typ = inst(dict.new(), poly)
          Ok(#(Var(typ, var), dict.new()))
        }
        Error(_) -> Error("Unbound variable " <> var)
      }
    c.App(fun, args) -> {
      // Generate a type variable for the return value
      let ret_type = TypeVar(new_type_var())

      // Infer the type of the function being applied
      use #(funexp, sub1) <- result.try(w(env, fun))

      // Infer the type of each argument
      use #(args_sub2, sub2) <- result.try(
        list.try_fold(args, #([], sub1), fn(acc, arg) {
          let #(l, sub1) = acc

          // Create a new environment with applied substitutions for recursive call
          let env1 = apply_sub_env(sub1, env)
          use #(texp2, sub2) <- result.try(w(env1, arg))

          // Apply the new substitutions to the previously inferred expressions
          let l = list.map(l, apply_sub_texpr(sub2, _))

          // Add the new expression and compose the substitutions
          Ok(#([texp2, ..l], compose_sub(sub2, sub1)))
        }),
      )

      // Apply the new substitutions to the function expression
      let funexp_sub2 = apply_sub_texpr(sub2, funexp)

      // Reverse the argument list to its original order
      let args_sub2 = list.reverse(args_sub2)

      // Create a function type from the argument types
      let arg_types_sub2 = list.map(args_sub2, fn(x) { x.typ })
      let args_funtype_sub2 = TypeFun(ret_type, arg_types_sub2)

      // Unify the inferred function type with the created function type
      use sub3 <- result.try(unify(funexp_sub2.typ, args_funtype_sub2))

      // Apply the latest substitutions to the function type and arguments
      let funtype_sub3 = apply_sub_texpr(sub3, funexp_sub2)
      let args_sub3 = list.map(args_sub2, apply_sub_texpr(sub3, _))

      // Apply all substitutions to the return type
      let sub123 = compose_sub(sub3, compose_sub(sub2, sub1))
      let ret_type = apply_sub(sub123, ret_type)

      Ok(#(App(ret_type, funtype_sub3, args_sub3), sub123))
    }
    c.Abs(params, body) -> {
      // Create a new type variable for each parameter
      let param_types =
        list.map(params, fn(x) { #(x, TypeVar(new_type_var())) })

      // Insert parameter types into the environment
      let body_env =
        list.fold(param_types, env, fn(env, item) {
          let #(var, var_type) = item
          dict.insert(env, var, Mono(var_type))
        })

      // Infer the type of the body in the new environment
      use #(body_texp, sub) <- result.try(w(body_env, body))

      // Update parameter types with substitutions from the body
      let param_types =
        list.map(param_types, fn(item) {
          let #(_, var_type) = item
          apply_sub(sub, var_type)
        })

      // TODO not sure if this is correct, but it fixes recursion
      // the real error might be elsewhere
      let body_texp = apply_sub_texpr(sub, body_texp)

      // Construct the function type
      let abs_type = TypeFun(body_texp.typ, param_types)

      Ok(#(Abs(abs_type, params, body_texp), sub))
    }
    c.Let(var, val, body) -> {
      // Infer the type of the value being bound
      use #(texp1, sub1) <- result.try(w(env, val))

      // Gleam does not generalize types here
      // let type1 = gen(apply_sub_env(sub1, env), texp1.typ)
      let type1 = Mono(texp1.typ)

      let env1 = dict.insert(env, var, type1)
      let env1 = apply_sub_env(sub1, env1)

      // Infer the type of the body within the updated environment
      use #(texp2, sub2) <- result.try(w(env1, body))

      // Combine the substitutions and return the type of the let expression
      Ok(#(Let(texp2.typ, var, texp1, texp2), compose_sub(sub2, sub1)))
    }
    c.If(cond, then_exp, else_exp) -> {
      // Infer the type of the condition, then, and else branch
      use #(texp_cond, sub_cond) <- result.try(w(env, cond))
      let env = apply_sub_env(sub_cond, env)

      use #(texp_then, sub_then) <- result.try(w(env, then_exp))
      let env = apply_sub_env(sub_then, env)
      let texp_cond = apply_sub_texpr(sub_then, texp_cond)

      use #(texp_else, sub_else) <- result.try(w(env, else_exp))
      let texp_cond = apply_sub_texpr(sub_else, texp_cond)
      let texp_then = apply_sub_texpr(sub_else, texp_then)

      // Ensure the condition is of type Bool
      case texp_cond.typ {
        TypeApp("Bool", []) -> {
          // Unify the types of the then and else branches
          use sub <- result.try(unify(texp_then.typ, texp_else.typ))

          // Apply the final substitutions to the if expression
          let if_type = apply_sub(sub, texp_then.typ)
          let texp_cond = apply_sub_texpr(sub, texp_cond)
          let texp_then = apply_sub_texpr(sub, texp_then)
          let texp_else = apply_sub_texpr(sub, texp_else)

          let sub =
            compose_sub(
              sub,
              compose_sub(sub_else, compose_sub(sub_then, sub_cond)),
            )

          Ok(#(If(if_type, texp_cond, texp_then, texp_else), sub))
        }
        _ -> Error("Condition expression must be of type Bool")
      }
    }
  }
}

pub fn w_module(env: Env, module: c.Module) -> Result(Module, String) {
  let module = c.unshadow_module(module)

  let funs = module.functions

  // Create an initial environment with type variables for each binding
  let fun_vars = list.map(module.functions, fn(x) { #(new_type_var(), x) })

  let initial_env =
    list.fold(fun_vars, env, fn(env, x) {
      let #(var, fun) = x
      dict.insert(env, fun.name, Poly(var, Mono(TypeVar(var))))
    })

  // Find mutually recursive functions and order of functions
  let groups =
    list.fold(funs, graph.new(), fn(g, fun) { graph.insert_node(g, fun.name) })
    |> list.fold(funs, _, fn(g, fun) { c.call_graph(g, fun.name, fun.body) })
    |> graph.strongly_connected_components()

  let fun_by_name =
    dict.from_list(list.map(fun_vars, fn(x) { #({ x.1 }.name, x) }))

  use #(functions, env, sub) <- result.try(
    list.try_fold(groups, #([], initial_env, dict.new()), fn(acc, group) {
      list.try_fold(group, acc, fn(acc, name) {
        let assert Ok(#(var, fun)) = dict.get(fun_by_name, name)

        let #(l, env, sub) = acc

        let fun_exp = c.Abs(fun.params, fun.body)

        use #(texp1, sub1) <- result.try(w(env, fun_exp))

        let assert Abs(fun_typ, params, body) = texp1
        let fun_type_gen = gen(apply_sub_env(sub1, env), fun_typ)
        let env1 = dict.insert(env, fun.name, fun_type_gen)
        let env1 = apply_sub_env(sub1, env1)

        // TODO is this line needed/correct?
        let sub1 = dict.insert(sub1, var, fun_typ)

        let combined_sub = compose_sub(sub1, sub)

        let l =
          list.map(l, fn(x) {
            let Function(name, vars, body, typ) = x
            let body = apply_sub_texpr(sub1, body)
            Function(name, vars, body, typ)
          })

        Ok(#(
          [Function(fun.name, params, body, fun_type_gen), ..l],
          env1,
          combined_sub,
        ))
      })
    }),
  )

  Ok(Module(types: module.types, functions: functions))
}

fn ftv(typ: Type) -> List(TypeVarName) {
  case typ {
    TypeVar(a) -> [a]
    TypeApp(_, args) -> list.flat_map(args, ftv)
    TypeFun(ret, args) -> list.flatten([ftv(ret), ..list.map(args, ftv)])
  }
}

fn ftv_poly(typ: Poly) -> List(TypeVarName) {
  case typ {
    Mono(typ) -> ftv(typ)
    Poly(var, typ) ->
      ftv_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

fn ftv_env(env: Env) -> List(TypeVarName) {
  dict.fold(env, [], fn(acc, _, val) { list.append(acc, ftv_poly(val)) })
}

fn ftv_typing(env: Env, typ: Poly) -> List(TypeVarName) {
  let env_vars = ftv_env(env)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

fn new_type_var() -> TypeVarName {
  unique_integer.mono_positive()
}

fn unify(t1: Type, t2: Type) -> Result(Sub, String) {
  case t1, t2 {
    TypeVar(var1), TypeVar(var2) if var1 == var2 -> Ok(dict.new())
    TypeVar(var), typ -> unify_var(var, typ)
    typ, TypeVar(var) -> unify_var(var, typ)
    TypeApp(typ1, args1), TypeApp(typ2, args2) if typ1 == typ2 ->
      unify_many(args1, args2)
    TypeFun(typ1, args1), TypeFun(typ2, args2) ->
      unify_many([typ1, ..args1], [typ2, ..args2])
    _, _ -> {
      Error("Types do not unify")
    }
  }
}

fn unify_many(types1: List(Type), types2: List(Type)) -> Result(Sub, String) {
  case types1, types2 {
    [head1, ..tail1], [head2, ..tail2] ->
      result.try(unify(head1, head2), fn(sub1) {
        let tail1 = list.map(tail1, fn(t) { apply_sub(sub1, t) })
        let tail2 = list.map(tail2, fn(t) { apply_sub(sub1, t) })
        result.try(unify_many(tail1, tail2), fn(sub2) {
          Ok(compose_sub(sub2, sub1))
        })
      })
    [], [] -> Ok(dict.new())
    _, _ -> Error("Type argument lists do not match")
  }
}

fn unify_var(var: TypeVarName, typ: Type) -> Result(Sub, String) {
  case typ {
    TypeVar(var2) if var == var2 -> Ok(dict.new())
    _ ->
      case occurs(var, typ) {
        True -> Error("Occurs check failed")
        False -> Ok(dict.insert(dict.new(), var, typ))
      }
  }
}

fn occurs(var: TypeVarName, typ: Type) -> Bool {
  list.contains(ftv(typ), var)
}

fn apply_sub(sub: Sub, typ: Type) -> Type {
  case typ {
    TypeVar(v) ->
      case dict.get(sub, v) {
        Ok(r) -> r
        _ -> typ
      }
    TypeApp(name, args) -> TypeApp(name, list.map(args, apply_sub(sub, _)))
    TypeFun(ret, args) ->
      TypeFun(apply_sub(sub, ret), list.map(args, apply_sub(sub, _)))
  }
}

fn apply_sub_poly(sub: Sub, typ: Poly) -> Poly {
  case typ {
    Mono(t) -> Mono(apply_sub(sub, t))
    Poly(a, t) -> Poly(a, apply_sub_poly(dict.delete(sub, a), t))
  }
}

fn apply_sub_env(sub: Sub, env: Env) -> Env {
  dict.map_values(env, fn(_, v) { apply_sub_poly(sub, v) })
}

fn compose_sub(sub1: Sub, sub2: Sub) -> Sub {
  let sub2_applied = dict.map_values(sub2, fn(_, typ) { apply_sub(sub1, typ) })
  dict.fold(sub1, sub2_applied, fn(acc, var, typ) { dict.insert(acc, var, typ) })
}
