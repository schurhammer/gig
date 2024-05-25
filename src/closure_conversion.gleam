import core.{
  type TExp, type TFunction, type TModule, type Type, Mono, TExpAbs, TExpApp,
  TExpIf, TExpInt, TExpLet, TExpVar, TFunction, TModule, TypeApp, TypeFun,
}

import env
import gleam/int
import gleam/io
import gleam/list

// TODO direct calls to top level functions could be added,
// but any higher-order usage probably need to be closure converted
// otherwise it could introduce problematic polymorphism
// especially in functions that return higher order functions

// assumptions:
// 1. renaming has been done such that there is no shadowing

type CC {
  CC(mod: TModule, uid: Int)
}

type Env =
  env.Env(String, Type)

pub fn cc_module(mod: TModule) {
  let c = CC(TModule([]), 1)

  // let global_env =
  //   list.fold(mod.functions, env.new(), fn(n, fun) {
  //     let assert Mono(t) = fun.typ
  //     env.put(n, fun.name, t)
  //   })

  let c =
    list.fold(mod.functions, c, fn(c, fun) {
      let #(c, e) = cc(c, env.new(), fun.body)
      let mod =
        TModule(functions: [TFunction(..fun, body: e), ..c.mod.functions])
      let c = CC(..c, mod: mod)
      c
    })
  c.mod
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: TExp) -> List(String) {
  case e {
    TExpInt(_, _) -> []
    TExpVar(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [var]
      }
    }
    TExpApp(typ, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    TExpAbs(typ, vars, exp) -> {
      let n = combine(vars, n)
      fv(n, exp)
    }
    TExpLet(typ, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, exp)
      combine(v, fv(n, exp))
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
  }
}

fn cc(c: CC, n: Env, e: TExp) -> #(CC, TExp) {
  case e {
    TExpInt(_, _) -> #(c, e)
    TExpVar(typ, var) -> {
      #(c, TExpVar(typ, var))
    }
    TExpApp(typ, fun, args) -> {
      let #(c, fun) = cc(c, n, fun)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, n, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, TExpApp(typ, fun, args))
    }
    TExpAbs(typ, vars, exp) -> {
      let #(c, exp) = cc(c, n, exp)

      let assert TypeFun(ret, param_types) = typ
      env.debug(n)

      let n =
        list.zip(vars, param_types)
        |> list.fold(n, fn(n, i) { env.put(n, i.0, i.1) })

      env.debug(n)

      // we ignore any vars not in env, we assume they are globally available
      // and do not need to enter the closure
      let closure_fields =
        list.filter_map(fv(vars, e), fn(v) { env.get_entry(n, v) })

      io.debug(closure_fields)

      // create global function
      let id = int.to_string(c.uid)
      let fun_name = "closure_C" <> id
      let env_name = "env_C" <> id
      let env_type_name = "env_type_C" <> id
      let env_type = TypeApp(env_type_name, [])

      let fun_params = [env_name, ..vars]

      // add let bindings to unpack the closure
      let fun_body =
        closure_fields
        |> list.fold(exp, fn(exp, field) {
          let #(name, typ) = field

          // function from env -> field
          let extract_fun_name = env_type_name <> "_GET_" <> name
          let extract_fun_type = TypeFun(typ, [env_type])
          let extract_fun = TExpVar(extract_fun_type, extract_fun_name)

          TExpLet(
            exp.typ,
            name,
            TExpApp(typ, extract_fun, [TExpVar(env_type, env_name)]),
            exp,
          )
        })

      // add global function to module
      let fun_type = TypeFun(fun_body.typ, [env_type, ..param_types])
      let fun = TFunction(fun_name, fun_params, fun_body, Mono(fun_type))
      let funs = [fun, ..c.mod.functions]
      let c = CC(TModule(funs), c.uid + 1)

      // create the closure
      let fun_pointer = TExpVar(typ, fun_name)

      let new_env_fun_name = env_type_name <> "_NEW"
      let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
      let new_env_fun_type = TypeFun(env_type, env_arg_types)
      let env_args = closure_fields |> list.map(fn(x) { TExpVar(x.1, x.0) })

      let env_constructor =
        TExpApp(env_type, TExpVar(new_env_fun_type, new_env_fun_name), env_args)

      let closure =
        TExpApp(typ, TExpVar(typ, "create_closure"), [
          fun_pointer,
          env_constructor,
        ])

      // TODO
      // create closure record type
      // figure out how create_closure works
      // figure out how to call closures
      // convert functions to closures when used in a higher order way

      #(c, closure)
    }
    TExpLet(typ, var, val, exp) -> {
      let #(c, val) = cc(c, n, val)
      let n = env.put(n, var, val.typ)
      let #(c, exp) = cc(c, n, exp)
      #(c, TExpLet(typ, var, val, exp))
    }
    TExpIf(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, n, cond)
      let #(c, then_e) = cc(c, n, then_e)
      let #(c, else_e) = cc(c, n, else_e)
      #(c, TExpIf(typ, cond, then_e, else_e))
    }
  }
}
