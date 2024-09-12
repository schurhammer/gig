import gig/core as t
import gig/env
import gig/gen_names
import gig/mono

import gleam/int
import gleam/list

type CC {
  CC(mod: Module, uid: Int)
}

pub type Module {
  Module(types: List(CustomType), functions: List(Function))
}

pub type CustomType {
  CustomType(name: String, variants: List(Variant), pointer: Bool)
}

pub type Variant {
  Variant(name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: t.Type)
}

pub type Function {
  Function(name: String, params: List(String), body: Exp, typ: t.Type)
}

pub type Exp {
  Literal(typ: t.Type, val: t.LiteralKind)
  Var(typ: t.Type, var: String)
  Call(typ: t.Type, fun: Exp, arg: List(Exp))
  CallClosure(typ: t.Type, fun: Exp, arg: List(Exp))
  Let(typ: t.Type, var: String, val: Exp, exp: Exp)
  If(typ: t.Type, cond: Exp, then_exp: Exp, else_exp: Exp)
  Panic(typ: t.Type, e: Exp)
}

pub fn cc_module(mod: mono.Context) {
  let c = CC(Module([], []), 1)

  let c =
    env.fold(mod.out.functions, c, fn(c, id, fun) {
      let #(c, e) = cc(c, fun.body)

      let parameters = list.map(fun.parameters, fn(x) { x.name })
      let function = Function(fun.id, parameters, e, fun.typ.typ)

      let mod = Module(c.mod.types, [function, ..c.mod.functions])
      CC(..c, mod: mod)
    })

  let c =
    env.fold(mod.out.types, c, fn(c, id, custom) {
      let variants =
        list.map(custom.variants, fn(v) {
          let fields =
            list.index_map(v.fields, fn(f, i) {
              Field(gen_names.get_field_name(i), f)
            })
          Variant(v.id, fields)
        })
      let pointer = case variants {
        [_] -> False
        _ -> True
      }
      let custom =
        CustomType(name: custom.id, variants: variants, pointer: pointer)

      let mod = Module(..c.mod, types: [custom, ..c.mod.types])
      CC(..c, mod: mod)
    })

  c.mod
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: t.Exp) -> List(#(String, t.Type)) {
  case e {
    t.Literal(_, _) -> []
    t.Local(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [#(var, typ)]
      }
    }
    t.Global(typ, var) -> []
    t.Call(typ, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    t.Fn(typ, vars, exp) -> {
      let vars = list.map(vars, fn(x) { x.name })
      let n = combine(vars, n)
      fv(n, exp)
    }
    t.Let(typ, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, exp)
      // TODO fv val?
      combine(v, fv(n, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
    t.Panic(typ, val) -> fv(n, val)
  }
}

fn cc(c: CC, e: t.Exp) -> #(CC, Exp) {
  case e {
    t.Literal(typ, var) -> #(c, Literal(typ, var))
    t.Local(typ, var) -> #(c, Var(typ, var))

    t.Global(t.FunctionType(..) as typ, var) -> {
      let val = Call(typ, Var(typ, "create_function"), [Var(typ, var)])
      #(c, val)
    }
    t.Global(typ, var) -> {
      #(c, Var(typ, var))
    }
    // detect "direct" function calls
    t.Call(typ, t.Global(fun_type, fun_name) as fun, args) -> {
      let fun = Var(fun_type, fun_name)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, Call(typ, fun, args))
    }
    t.Call(typ, fun, args) -> {
      let #(c, fun) = cc(c, fun)
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, CallClosure(typ, fun, args))
    }
    t.Fn(typ, vars, exp) -> {
      // TODO recursive call before or after converting this?
      let #(c, exp) = cc(c, exp)

      let var_names = list.map(vars, fn(x) { x.name })
      let var_types = list.map(vars, fn(x) { x.typ })

      let closure_fields = fv(var_names, e)

      case closure_fields {
        [] -> {
          // create global function
          let id = int.to_string(c.uid)
          let fun_name = "Closure_" <> id
          let funs = c.mod.functions
          let fun = Function(fun_name, var_names, exp, typ)
          let funs = [fun, ..funs]
          let c = CC(Module(c.mod.types, funs), c.uid + 1)

          // make a closure reference to the function
          let val = Call(typ, Var(typ, "create_function"), [Var(typ, fun_name)])
          #(c, val)
        }
        _ -> {
          // create global function
          let id = int.to_string(c.uid)
          let fun_name = "Closure_" <> id
          let env_name = "ClosureEnv_" <> id
          let env_type = t.NamedType(env_name, [])

          let fun_params = [env_name, ..var_names]

          // add let bindings to unpack the closure
          let fun_body =
            closure_fields
            |> list.index_fold(exp, fn(exp, field, i) {
              let #(name, typ) = field

              // function from env -> field
              let extract_fun_name = gen_names.get_getter_name(env_name, i)
              let extract_fun_type = t.FunctionType([env_type], typ)
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
          let fun_type = t.FunctionType([env_type, ..var_types], fun_body.typ)
          let fun = Function(fun_name, fun_params, fun_body, fun_type)
          let funs = [fun, ..funs]

          // create the closure object
          let fun_pointer = Var(typ, fun_name)

          let new_env_fun_name = gen_names.get_constructor_name(env_name)
          let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
          let new_env_fun_type = t.FunctionType(env_arg_types, env_type)
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
          let typedef = CustomType(env_name, [variant], True)

          let types = [typedef, ..types]

          let c = CC(Module(types, funs), c.uid + 1)

          #(c, closure)
        }
      }
    }
    t.Let(typ, var, val, exp) -> {
      let #(c, val) = cc(c, val)
      let #(c, exp) = cc(c, exp)
      #(c, Let(typ, var, val, exp))
    }
    t.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, cond)
      let #(c, then_e) = cc(c, then_e)
      let #(c, else_e) = cc(c, else_e)
      #(c, If(typ, cond, then_e, else_e))
    }
    t.Panic(typ, val) -> {
      let #(c, val) = cc(c, val)
      #(c, Panic(typ, val))
    }
  }
}
