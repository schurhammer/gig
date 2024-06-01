import env
import gleam/int
import gleam/list
import monomorphise.{
  type CustomType, type Mono, CustomType, Field, MonoApp, MonoFun, Variant,
} as mono

// assumptions:
// 1. renaming has been done such that there is no shadowing

type Env =
  env.Env(String, Mono)

type CC {
  CC(mod: Module, uid: Int)
}

pub type Module {
  Module(types: List(CustomType), functions: List(Function))
}

pub type Function {
  Function(name: String, params: List(String), body: Exp, typ: Mono)
}

pub type Exp {
  Int(typ: Mono, val: String)
  Var(typ: Mono, var: String)
  Call(typ: Mono, fun: Exp, arg: List(Exp))
  CallClosure(typ: Mono, fun: Exp, arg: List(Exp))
  Let(typ: Mono, var: String, val: Exp, exp: Exp)
  If(typ: Mono, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub fn cc_module(mod: mono.Module) {
  let c = CC(Module(types: mod.types, functions: []), 1)

  let c =
    list.fold(mod.functions, c, fn(c, fun) {
      let assert MonoFun(ret, param_types) = fun.typ
      let n =
        list.zip(fun.params, param_types)
        |> list.fold(env.new(), fn(n, i) { env.put(n, i.0, i.1) })

      let #(c, e) = cc(c, n, fun.body)

      let function = Function(fun.name, fun.params, e, fun.typ)

      let mod = Module(c.mod.types, [function, ..c.mod.functions])
      CC(..c, mod: mod)
    })
  c.mod
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: mono.Exp) -> List(String) {
  case e {
    mono.Int(_, _) -> []
    mono.Var(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [var]
      }
    }
    mono.Call(typ, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    mono.Fn(typ, vars, exp) -> {
      let n = combine(vars, n)
      fv(n, exp)
    }
    mono.Let(typ, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, exp)
      combine(v, fv(n, exp))
    }
    mono.If(typ, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
  }
}

fn cc(c: CC, n: Env, e: mono.Exp) -> #(CC, Exp) {
  case e {
    mono.Int(typ, var) -> #(c, Int(typ, var))
    // detect functions that need to be converted to closures
    mono.Var(MonoFun(ret, params) as typ, var) -> {
      case env.has(n, var) {
        // not in local env so it must be a global function
        False -> {
          // TODO convert!! ??
          let null_env = Int(MonoApp("Int", []), "0")
          let val =
            Call(typ, Var(typ, "create_closure"), [Var(typ, var), null_env])
          #(c, val)
        }
        // otherwise its a closure
        True -> {
          #(c, Var(typ, var))
        }
      }
    }
    mono.Var(typ, var) -> {
      #(c, Var(typ, var))
    }
    // detect "direct" function calls
    mono.Call(typ, mono.Var(fun_type, fun_name) as fun, args) -> {
      case env.has(n, fun_name) {
        // not in local env so it must be a global function
        False -> {
          let fun = Var(fun_type, fun_name)
          let #(c, args) =
            list.fold(args, #(c, []), fn(acc, arg) {
              let #(c, args) = acc
              let #(c, arg) = cc(c, n, arg)
              #(c, [arg, ..args])
            })
          let args = list.reverse(args)
          #(c, Call(typ, fun, args))
        }
        // otherwise its a closure
        True -> {
          let #(c, fun) = cc(c, n, fun)
          let #(c, args) =
            list.fold(args, #(c, []), fn(acc, arg) {
              let #(c, args) = acc
              let #(c, arg) = cc(c, n, arg)
              #(c, [arg, ..args])
            })
          let args = list.reverse(args)
          #(c, CallClosure(typ, fun, args))
        }
      }
    }
    mono.Call(typ, fun, args) -> {
      let #(c, fun) = cc(c, n, fun)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, n, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, CallClosure(typ, fun, args))
    }
    mono.Fn(typ, vars, exp) -> {
      // update the env with abstraction vars
      let assert MonoFun(ret, param_types) = typ
      let n =
        list.zip(vars, param_types)
        |> list.fold(n, fn(n, i) { env.put(n, i.0, i.1) })

      // TODO recursive call before or after converting this?
      let #(c, exp) = cc(c, n, exp)

      // we ignore any vars not in env, we assume they are globally available
      // and do not need to enter the closure
      let closure_fields =
        list.filter_map(fv(vars, e), fn(v) { env.get_entry(n, v) })

      // create global function
      let id = int.to_string(c.uid)
      let fun_name = "Closure_" <> id
      let env_name = "Env_" <> id
      let env_type = MonoApp(env_name, [])

      let fun_params = [env_name, ..vars]

      // add let bindings to unpack the closure
      let fun_body =
        closure_fields
        |> list.fold(exp, fn(exp, field) {
          let #(name, typ) = field

          // function from env -> field
          let extract_fun_name = env_name <> "_" <> name
          let extract_fun_type = MonoFun(typ, [env_type])
          let extract_fun = Var(extract_fun_type, extract_fun_name)

          Let(
            exp.typ,
            name,
            Call(typ, extract_fun, [Var(env_type, env_name)]),
            exp,
          )
        })

      let funs = c.mod.functions
      let types = c.mod.types

      // add cloure function to module
      let fun_type = MonoFun(fun_body.typ, [env_type, ..param_types])
      let fun = Function(fun_name, fun_params, fun_body, fun_type)
      let funs = [fun, ..funs]

      // create the closure object
      let fun_pointer = Var(typ, fun_name)

      let new_env_fun_name = env_name <> ""
      let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
      let new_env_fun_type = MonoFun(env_type, env_arg_types)
      let env_args = list.map(closure_fields, fn(x) { Var(x.1, x.0) })

      let env_constructor_call =
        Call(env_type, Var(new_env_fun_type, new_env_fun_name), env_args)

      let closure =
        Call(typ, Var(typ, "create_closure"), [
          fun_pointer,
          env_constructor_call,
        ])

      let fields = list.map(closure_fields, fn(x) { Field(x.0, x.1) })
      let variant = Variant(env_name, fields)
      let typedef = CustomType(env_name, [], [variant])

      let types = [typedef, ..types]

      let c = CC(Module(types, funs), c.uid + 1)

      #(c, closure)
    }
    mono.Let(typ, var, val, exp) -> {
      let #(c, val) = cc(c, n, val)
      let n = env.put(n, var, val.typ)
      let #(c, exp) = cc(c, n, exp)
      #(c, Let(typ, var, val, exp))
    }
    mono.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, n, cond)
      let #(c, then_e) = cc(c, n, then_e)
      let #(c, else_e) = cc(c, n, else_e)
      #(c, If(typ, cond, then_e, else_e))
    }
  }
}
