import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import unique_integer

pub type ExpVar =
  String

pub type Module {
  Module(types: List(TypeDef), functions: List(Function))
}

pub type TypeDef {
  TypeDef(name: String, params: List(String), body: List(VariantDef))
}

pub type VariantDef {
  VariantDef(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: String)
}

pub type Function {
  Function(name: ExpVar, body: Exp)
}

pub type Exp {
  ExpInt(val: Int)
  ExpVar(var: ExpVar)
  ExpApp(fun: Exp, args: List(Exp))
  ExpAbs(var: List(ExpVar), exp: Exp)
  ExpLet(var: ExpVar, val: Exp, exp: Exp)
  ExpIf(cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type TypeVar =
  Int

pub type TModule {
  TModule(functions: List(TFunction))
}

pub type TFunction {
  TFunction(name: ExpVar, body: TExp)
}

pub type TExp {
  TExpInt(typ: Type, val: Int)
  TExpVar(typ: Type, var: ExpVar)
  TExpApp(typ: Type, fun: TExp, arg: List(TExp))
  TExpAbs(typ: Type, var: List(ExpVar), exp: TExp)
  TExpLet(typ: Type, var: ExpVar, val: TExp, exp: TExp)
  TExpIf(typ: Type, cond: TExp, then_exp: TExp, else_exp: TExp)
}

pub type Type {
  TypeVar(var: TypeVar)
  TypeApp(typ: String, args: List(Type))
  TypeFun(ret: Type, args: List(Type))
}

pub type Poly {
  Mono(typ: Type)
  Poly(var: TypeVar, typ: Poly)
}

pub type Env =
  dict.Dict(ExpVar, Poly)

pub type Sub =
  dict.Dict(TypeVar, Type)

fn ftv(typ: Type) -> List(TypeVar) {
  case typ {
    TypeVar(a) -> [a]
    TypeApp(_, args) -> list.flat_map(args, ftv)
    TypeFun(ret, args) -> list.flatten([ftv(ret), ..list.map(args, ftv)])
  }
}

fn ftv_poly(typ: Poly) -> List(TypeVar) {
  case typ {
    Mono(typ) -> ftv(typ)
    Poly(var, typ) ->
      ftv_poly(typ)
      |> list.filter(fn(x) { x != var })
  }
}

fn ftv_env(env: Env) -> List(TypeVar) {
  dict.fold(env, [], fn(acc, _, val) { list.append(acc, ftv_poly(val)) })
}

fn ftv_typing(env: Env, typ: Poly) -> List(TypeVar) {
  let env_vars = ftv_env(env)
  ftv_poly(typ)
  |> list.filter(fn(x) { !list.contains(env_vars, x) })
}

fn new_type_var() -> TypeVar {
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
      io.println_error("\n\nfailed to unify\n")
      io.debug(t1)
      io.debug(t2)
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

fn unify_var(var: TypeVar, typ: Type) -> Result(Sub, String) {
  case typ {
    TypeVar(var2) if var == var2 -> Ok(dict.new())
    _ ->
      case occurs(var, typ) {
        True -> Error("Occurs check failed")
        False -> Ok(dict.insert(dict.new(), var, typ))
      }
  }
}

fn occurs(var: TypeVar, typ: Type) -> Bool {
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

fn apply_sub_texpr(sub: Sub, texp: TExp) -> TExp {
  case texp {
    TExpInt(_, _) -> texp
    TExpVar(typ, var) -> TExpVar(apply_sub(sub, typ), var)
    TExpApp(typ, fun, arg) ->
      TExpApp(
        apply_sub(sub, typ),
        apply_sub_texpr(sub, fun),
        list.map(arg, apply_sub_texpr(sub, _)),
      )
    TExpAbs(typ, var, exp) ->
      TExpAbs(apply_sub(sub, typ), var, apply_sub_texpr(sub, exp))
    TExpLet(typ, var, val, exp) ->
      TExpLet(
        apply_sub(sub, typ),
        var,
        apply_sub_texpr(sub, val),
        apply_sub_texpr(sub, exp),
      )
    TExpIf(typ, cond, then_exp, else_exp) ->
      TExpIf(
        apply_sub(sub, typ),
        apply_sub_texpr(sub, cond),
        apply_sub_texpr(sub, then_exp),
        apply_sub_texpr(sub, else_exp),
      )
  }
}

fn compose_sub(sub1: Sub, sub2: Sub) -> Sub {
  let sub2_applied = dict.map_values(sub2, fn(_, typ) { apply_sub(sub1, typ) })
  dict.fold(sub1, sub2_applied, fn(acc, var, typ) { dict.insert(acc, var, typ) })
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

pub fn w(env: Env, exp: Exp) -> Result(#(TExp, Sub), String) {
  io.println_error("\n--")
  io.debug(exp)

  case exp {
    ExpInt(val) -> Ok(#(TExpInt(TypeApp("Int", []), val), dict.new()))
    ExpVar(var) ->
      case dict.get(env, var) {
        Ok(poly) -> {
          // Instantiate the polymorphic type to get a monomorphic type
          let typ = inst(dict.new(), poly)
          Ok(#(TExpVar(typ, var), dict.new()))
        }
        Error(_) -> Error("Unbound variable " <> var)
      }
    ExpApp(fun, args) -> {
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

      Ok(#(TExpApp(ret_type, funtype_sub3, args_sub3), sub123))
    }
    ExpAbs(params, body) -> {
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

      // Construct the function type
      let abs_type = TypeFun(body_texp.typ, param_types)

      Ok(#(TExpAbs(abs_type, params, body_texp), sub))
    }
    ExpLet(var, val, body) -> {
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
      Ok(#(TExpLet(texp2.typ, var, texp1, texp2), compose_sub(sub2, sub1)))
    }
    ExpIf(cond, then_exp, else_exp) -> {
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

          Ok(#(TExpIf(if_type, texp_cond, texp_then, texp_else), sub))
        }
        _ -> Error("Condition expression must be of type Bool")
      }
    }
  }
  |> io.debug()
}

fn rename_var(replace: String, with: String, in: Exp) -> Exp {
  case in {
    ExpInt(_) -> in
    ExpVar(var) ->
      case var == replace {
        True -> ExpVar(with)
        False -> in
      }
    ExpApp(fun, args) -> {
      let fun = rename_var(replace, with, fun)
      let args = list.map(args, rename_var(replace, with, _))
      ExpApp(fun, args)
    }
    ExpAbs(vars, exp) ->
      case list.contains(vars, replace) {
        True -> in
        False -> ExpAbs(vars, rename_var(replace, with, exp))
      }
    ExpLet(var, val, exp) ->
      case var == replace {
        True -> ExpLet(var, rename_var(replace, with, val), exp)
        False ->
          ExpLet(
            var,
            rename_var(replace, with, val),
            rename_var(replace, with, exp),
          )
      }
    ExpIf(cond, then_exp, else_exp) ->
      ExpIf(
        rename_var(replace, with, cond),
        rename_var(replace, with, then_exp),
        rename_var(replace, with, else_exp),
      )
  }
}

fn unshadow(taken: List(String), i: Int, exp: Exp) {
  case exp {
    ExpInt(_) -> exp
    ExpVar(_) -> exp
    ExpApp(fun, args) -> {
      let fun = unshadow(taken, i, fun)
      let args = list.map(args, unshadow(taken, i, _))
      ExpApp(fun, args)
    }
    ExpAbs(vars, exp) -> {
      let #(_, _, vars, exp) =
        list.fold(vars, #(taken, i, [], exp), fn(acc, var) {
          let #(taken, i, vars, exp) = acc
          case list.contains(taken, var) {
            True -> {
              let new_var = "V" <> int.to_string(i) <> "_" <> var
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
      let vars = list.reverse(vars)
      ExpAbs(vars, exp)
    }
    ExpLet(var, val, exp) ->
      case list.contains(taken, var) {
        True -> {
          let val = unshadow(taken, i, val)
          let new_var = "V" <> int.to_string(i) <> "_" <> var
          let i = i + 1
          let exp = rename_var(var, new_var, exp)
          let var = new_var
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          ExpLet(var, val, exp)
        }
        False -> {
          let val = unshadow(taken, i, val)
          let taken = [var, ..taken]
          let exp = unshadow(taken, i, exp)
          ExpLet(var, val, exp)
        }
      }
    ExpIf(cond, then_exp, else_exp) -> {
      let cond = unshadow(taken, i, cond)
      let then_exp = unshadow(taken, i, then_exp)
      let else_exp = unshadow(taken, i, else_exp)
      ExpIf(cond, then_exp, else_exp)
    }
  }
}

fn unshadow_module(module: Module) -> Module {
  let fun_names = list.map(module.functions, fn(fun) { fun.name })
  let functions =
    list.map(module.functions, fn(fun) {
      Function(fun.name, unshadow(fun_names, 1, fun.body))
    })
  let types = module.types
  Module(types, functions)
}

pub fn w_module(env: Env, module: Module) -> Result(TModule, String) {
  let module = unshadow_module(module)

  // Create an initial environment with type variables for each binding
  let funs = list.map(module.functions, fn(x) { #(new_type_var(), x) })

  let initial_env =
    list.fold(funs, env, fn(env, x) {
      let #(var, fun) = x
      dict.insert(env, fun.name, Poly(var, Mono(TypeVar(var))))
    })

  // TODO I think we need to order the functions
  // 1. infer functions called by the current function first
  // 2. infer mutually recursive functions together
  // 3. maybe there is some recursive solution to this

  // Infer types for all binding expressions
  use #(functions, env, sub) <- result.try(
    list.try_fold(funs, #([], initial_env, dict.new()), fn(acc, x) {
      let #(var, fun) = x
      let #(l, env, sub) = acc

      use #(texp1, sub1) <- result.try(w(env, fun.body))

      // let type1 = gen(apply_sub_env(sub1, env), texp1.typ)

      // let env1 = dict.insert(env, fun.name, type1)
      // let env1 = apply_sub_env(sub1, env1)
      let env1 = apply_sub_env(sub1, env)

      let sub1 = dict.insert(sub1, var, texp1.typ)
      let combined_sub = compose_sub(sub1, sub)

      let l =
        list.map(l, fn(x) {
          let TFunction(name, texp) = x
          let texp = apply_sub_texpr(sub1, texp)
          TFunction(name, texp)
        })

      Ok(#([TFunction(fun.name, texp1), ..l], env1, combined_sub))
    }),
  )

  Ok(TModule(functions: functions))
}

fn compile_type_name(typ: Type) -> String {
  case typ {
    TypeVar(_) -> panic as "type vars should be resolved"
    TypeApp(name, args) ->
      string.join([name, ..list.map(args, compile_type_name)], "_")
    TypeFun(..) -> todo as "todo function types"
  }
}

fn hit_target(target: String, with: String) {
  case target {
    "" -> with
    // TODO not sure if this is always valid
    // "RETURN" -> "return " <> with <> ";\n"
    target -> target <> " = " <> with <> ";\n"
  }
}

fn compile_texp(arg: TExp, target: String, id: Int) -> String {
  case arg {
    TExpInt(_, val) -> hit_target(target, int.to_string(val))

    TExpVar(_, val) -> hit_target(target, val)
    TExpApp(typ, fun, args) ->
      hit_target(
        target,
        compile_texp(fun, "", id)
          <> "("
          <> list.map(args, compile_texp(_, "", id)) |> string.join(", ")
          <> ")",
      )

    TExpAbs(..) -> panic as "functions should be lifted"
    TExpLet(typ, var, val, exp) ->
      compile_type_name(typ)
      <> " "
      <> var
      <> ";\n"
      <> compile_texp(val, var, id)
      <> compile_texp(exp, target, id)
    TExpIf(typ, cond, then_exp, else_exp) ->
      "if ("
      <> compile_texp(cond, "", id)
      <> ") {\n"
      <> compile_texp(then_exp, target, id + 1)
      <> "} else {\n"
      <> compile_texp(else_exp, target, id + 1)
      <> "}\n"
  }
}

import gleam/io

fn compile_function(fun: TFunction) -> String {
  let assert TExpAbs(typ, params, body) = fun.body
  let assert TypeFun(ret, param_types) = typ
  compile_type_name(ret)
  <> " "
  <> fun.name
  <> "("
  <> list.zip(params, param_types)
  |> list.map(fn(p) {
    let #(name, typ) = p
    compile_type_name(typ) <> " " <> name
  })
  |> string.join(", ")
  <> ") {\n"
  <> compile_type_name(ret)
  <> " RETURN;\n"
  <> compile_texp(body, "RETURN", 1)
  <> "return RETURN;\n"
  <> "}"
}

pub fn compile_module(mod: TModule) -> String {
  list.map(mod.functions, compile_function)
  |> string.join("\n\n")
}

pub fn pretty_print_type(typ: Type) -> String {
  case typ {
    TypeVar(var) -> format_type_var(var)
    TypeApp(name, args) -> format_type_app(name, args)
    TypeFun(ret, args) -> format_function_type(args, ret)
  }
}

pub fn pretty_print_exp(exp: Exp) -> String {
  case exp {
    ExpInt(val) -> int.to_string(val)
    ExpVar(var) -> var
    ExpApp(fun, arg) ->
      case fun {
        ExpVar(var) -> var
        _ -> "{" <> pretty_print_exp(fun) <> "}"
      }
      <> "("
      <> string.join(list.map(arg, pretty_print_exp), ", ")
      <> ")"
    ExpAbs(var, exp) ->
      "fn(" <> string.join(var, ", ") <> ") {" <> pretty_print_exp(exp) <> "}"
    ExpLet(var, val, exp) ->
      "(let "
      <> var
      <> " = "
      <> pretty_print_exp(val)
      <> " in "
      <> pretty_print_exp(exp)
      <> ")"

    ExpIf(cond, then_exp, else_exp) ->
      "if ("
      <> pretty_print_exp(cond)
      <> ") {"
      <> pretty_print_exp(then_exp)
      <> "} else {"
      <> pretty_print_exp(else_exp)
      <> "}"
  }
}

pub fn pretty_print_texp(texp: TExp) -> String {
  case texp {
    TExpInt(_, val) -> int.to_string(val)
    TExpVar(_, var) -> var
    TExpApp(_, fun, arg) ->
      case fun {
        TExpVar(_, var) -> var
        _ -> "{" <> pretty_print_texp(fun) <> "}"
      }
      <> "("
      <> string.join(list.map(arg, pretty_print_texp), ", ")
      <> ")"
    TExpAbs(typ, var, exp) ->
      "fn("
      <> case typ {
        TypeFun(ret, args) -> {
          list.zip(var, args)
          |> list.map(fn(x) { x.0 <> ": " <> pretty_print_type(x.1) })
          |> string.join(", ")
          <> ")"
          <> " -> "
          <> pretty_print_type(ret)
        }
        _ -> "<err>)"
      }
      <> " {"
      <> pretty_print_texp(exp)
      <> "}"
    TExpLet(_, var, val, exp) ->
      "(let "
      <> var
      <> " = "
      <> pretty_print_texp(val)
      <> " in "
      <> pretty_print_texp(exp)
      <> ")"
    TExpIf(_, cond, then_exp, else_exp) ->
      "if ("
      <> pretty_print_texp(cond)
      <> ") {"
      <> pretty_print_texp(then_exp)
      <> "} else {"
      <> pretty_print_texp(else_exp)
      <> "}"
  }
}

pub fn normalize_vars_type(
  typ: Type,
  sub: dict.Dict(TypeVar, TypeVar),
) -> #(Type, dict.Dict(TypeVar, TypeVar)) {
  case typ {
    TypeVar(var) -> {
      case dict.get(sub, var) {
        Ok(new_var) -> #(TypeVar(new_var), sub)
        Error(_) -> {
          let new_var = dict.size(sub) + 1
          let new_sub = dict.insert(sub, var, new_var)
          #(TypeVar(new_var), new_sub)
        }
      }
    }
    TypeApp(name, args) -> {
      let result =
        args
        |> list.reverse
        |> list.fold(#([], sub), fn(acc, arg) {
          let #(l, sub) = acc
          let #(new_arg, new_sub) = normalize_vars_type(arg, sub)
          #([new_arg, ..l], new_sub)
        })
      #(TypeApp(name, result.0), result.1)
    }
    TypeFun(ret, args) -> {
      let #(args, sub) =
        args
        |> list.reverse
        |> list.fold(#([], sub), fn(acc, arg) {
          let #(l, sub) = acc
          let #(new_arg, new_sub) = normalize_vars_type(arg, sub)
          #([new_arg, ..l], new_sub)
        })
      let #(ret, sub) = normalize_vars_type(ret, sub)
      #(TypeFun(ret, args), sub)
    }
  }
}

pub fn normalize_vars_poly(
  typ: Poly,
  sub: dict.Dict(TypeVar, TypeVar),
) -> #(Poly, dict.Dict(TypeVar, TypeVar)) {
  case typ {
    Mono(t) -> {
      let #(m, s) = normalize_vars_type(t, sub)
      #(Mono(m), s)
    }
    Poly(var, t) ->
      case dict.get(sub, var) {
        Ok(var) -> {
          let #(inner, sub) = normalize_vars_poly(t, sub)
          #(Poly(var, inner), sub)
        }
        Error(_) -> {
          let new_var = dict.size(sub) + 1
          let sub = dict.insert(sub, var, new_var)
          let #(inner, sub) = normalize_vars_poly(t, sub)
          #(Poly(new_var, inner), sub)
        }
      }
  }
}

pub fn normalize_vars_texp(
  texp: TExp,
  sub: dict.Dict(TypeVar, TypeVar),
) -> #(TExp, dict.Dict(TypeVar, TypeVar)) {
  case texp {
    TExpInt(typ, val) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      #(TExpInt(new_typ, val), new_sub)
    }
    TExpVar(typ, var) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      #(TExpVar(new_typ, var), new_sub)
    }
    TExpApp(typ, fun, args) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_fun, new_sub) = normalize_vars_texp(fun, new_sub)
      let #(new_args, new_sub) =
        list.fold(args, #([], new_sub), fn(acc, arg) {
          let #(l, new_sub) = acc
          let #(new_arg, new_sub) = normalize_vars_texp(arg, new_sub)
          #(list.append(l, [new_arg]), new_sub)
        })
      #(TExpApp(new_typ, new_fun, new_args), new_sub)
    }
    TExpAbs(typ, var, exp) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_exp, new_sub) = normalize_vars_texp(exp, new_sub)
      #(TExpAbs(new_typ, var, new_exp), new_sub)
    }
    TExpLet(typ, var, val, exp) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_val, new_sub) = normalize_vars_texp(val, new_sub)
      let #(new_exp, new_sub) = normalize_vars_texp(exp, new_sub)
      #(TExpLet(new_typ, var, new_val, new_exp), new_sub)
    }
    TExpIf(typ, cond, then_exp, else_exp) -> {
      let #(new_typ, new_sub) = normalize_vars_type(typ, sub)
      let #(new_cond, new_sub) = normalize_vars_texp(cond, new_sub)
      let #(new_then, new_sub) = normalize_vars_texp(then_exp, new_sub)
      let #(new_else, new_sub) = normalize_vars_texp(else_exp, new_sub)
      #(TExpIf(new_typ, new_cond, new_then, new_else), new_sub)
    }
  }
}

fn format_type_var(var: TypeVar) -> String {
  "t" <> int.to_string(var)
}

fn format_function_type(args: List(Type), result: Type) -> String {
  "fn("
  <> list.map(args, pretty_print_type)
  |> string.join(", ")
  <> ")"
  <> " -> "
  <> pretty_print_type(result)
}

fn format_type_app(name: String, args: List(Type)) -> String {
  let args_str =
    args
    |> list.map(pretty_print_type)
    |> string.join(" ")
  case args_str {
    "" -> name
    s -> name <> " " <> s
  }
}
