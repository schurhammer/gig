import core.{type Type, Field, TypeApp, TypeDef, TypeFun, VariantDef}
import env
import gleam/int
import gleam/list
import typed as t

// TODO direct calls to top level functions could be added,
// but any higher-order usage probably need to be closure converted
// otherwise it could introduce problematic polymorphism
// especially in functions that return higher order functions

// assumptions:
// 1. renaming has been done such that there is no shadowing

type Env =
  env.Env(String, Type)

type CC {
  CC(mod: Module, uid: Int)
}

pub type Module {
  Module(types: List(core.CustomType), functions: List(Function))
}

pub type Function {
  Function(name: String, params: List(String), body: Exp, typ: core.Type)
}

pub type Exp {
  Int(typ: Type, val: Int)
  Var(typ: Type, var: String)
  Call(typ: Type, fun: Exp, arg: List(Exp))
  CallClosure(typ: Type, fun: Exp, arg: List(Exp))
  Let(typ: Type, var: String, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub fn cc_module(mod: t.Module) {
  let c = CC(Module(types: mod.types, functions: []), 1)

  let c =
    list.fold(mod.functions, c, fn(c, fun) {
      let assert t.Mono(TypeFun(ret, param_types)) = fun.typ
      let n =
        list.zip(fun.params, param_types)
        |> list.fold(env.new(), fn(n, i) { env.put(n, i.0, i.1) })

      let #(c, e) = cc(c, n, fun.body)

      let assert t.Mono(typ) = fun.typ
      let function = Function(fun.name, fun.params, e, typ)

      let mod = Module(c.mod.types, [function, ..c.mod.functions])
      CC(..c, mod: mod)
    })
  c.mod
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: t.Exp) -> List(String) {
  case e {
    t.Int(_, _) -> []
    t.Var(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [var]
      }
    }
    t.Call(typ, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    t.Fn(typ, vars, exp) -> {
      let n = combine(vars, n)
      fv(n, exp)
    }
    t.Let(typ, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, exp)
      combine(v, fv(n, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
  }
}

fn cc(c: CC, n: Env, e: t.Exp) -> #(CC, Exp) {
  case e {
    t.Int(typ, var) -> #(c, Int(typ, var))
    // detect functions that need to be converted to closures
    t.Var(TypeFun(ret, params) as typ, var) -> {
      case env.has(n, var) {
        // not in local env so it must be a global function
        False -> {
          // TODO convert!!
          let null_env = Int(TypeApp("Int", []), 0)
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
    t.Var(typ, var) -> {
      #(c, Var(typ, var))
    }
    // detect "direct" function calls
    t.Call(typ, t.Var(fun_type, fun_name) as fun, args) -> {
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
    t.Call(typ, fun, args) -> {
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
    t.Fn(typ, vars, exp) -> {
      // update the env with abstraction vars
      let assert TypeFun(ret, param_types) = typ
      let n =
        list.zip(vars, param_types)
        |> list.fold(n, fn(n, i) { env.put(n, i.0, i.1) })

      // TODO recursive call before or after converting this?
      let #(c, exp) = cc(c, n, exp)

      // we ignore any vars not in env, we assume they are globally available
      // and do not need to enter the closure
      let closure_fields =
        list.filter_map(fv(vars, e), fn(v) { env.get_entry(n, v) })

      // io.debug(closure_fields)

      // create global function
      let id = int.to_string(c.uid)
      let fun_name = "closure_C" <> id
      let env_name = "env_C" <> id
      let env_type_name = "env_type_C" <> id
      let env_type = TypeApp(env_type_name <> "*", [])

      let fun_params = [env_name, ..vars]

      // add let bindings to unpack the closure
      let fun_body =
        closure_fields
        |> list.fold(exp, fn(exp, field) {
          let #(name, typ) = field

          // function from env -> field
          let extract_fun_name = env_type_name <> "_GET_" <> name
          let extract_fun_type = TypeFun(typ, [env_type])
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
      let fun_type = TypeFun(fun_body.typ, [env_type, ..param_types])
      let fun = Function(fun_name, fun_params, fun_body, fun_type)
      let funs = [fun, ..funs]

      // create the closure object
      let fun_pointer = Var(typ, fun_name)

      let new_env_fun_name = env_type_name <> "_NEW"
      let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
      let new_env_fun_type = TypeFun(env_type, env_arg_types)
      let env_args = list.map(closure_fields, fn(x) { Var(x.1, x.0) })

      let env_constructor_call =
        Call(env_type, Var(new_env_fun_type, new_env_fun_name), env_args)

      let closure =
        Call(typ, Var(typ, "create_closure"), [
          fun_pointer,
          env_constructor_call,
        ])

      let fields = list.map(closure_fields, fn(x) { Field(x.0, x.1) })
      let variant = VariantDef(env_type_name, fields)
      let typedef = TypeDef(env_type_name, [], [variant])

      let types = [typedef, ..types]

      let c = CC(Module(types, funs), c.uid + 1)

      #(c, closure)
    }
    t.Let(typ, var, val, exp) -> {
      let #(c, val) = cc(c, n, val)
      let n = env.put(n, var, val.typ)
      let #(c, exp) = cc(c, n, exp)
      #(c, Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, n, cond)
      let #(c, then_e) = cc(c, n, then_e)
      let #(c, else_e) = cc(c, n, else_e)
      #(c, If(typ, cond, then_e, else_e))
    }
  }
}
