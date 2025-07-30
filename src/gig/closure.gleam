import gig/core
import gig/gen_names
import gig/mono
import gleam/dict

import gleam/int
import gleam/list

type CC {
  CC(mod: Module, uid: Int)
}

pub type Module {
  Module(
    types: List(CustomType),
    functions: List(Function),
    externals: List(Function),
  )
}

pub type CustomType {
  CustomType(
    name: String,
    display_name: String,
    variants: List(Variant),
    pointer: Bool,
  )
}

pub type Variant {
  Variant(name: String, display_name: String, fields: List(Field))
}

pub type Field {
  Field(name: String, typ: core.Type)
}

pub type Function {
  Function(name: String, params: List(String), body: Exp, typ: core.Type)
}

pub type Exp {
  Literal(typ: core.Type, val: core.LiteralKind)
  Var(typ: core.Type, var: String)
  Call(typ: core.Type, fun: Exp, arg: List(Exp))
  CallClosure(typ: core.Type, fun: Exp, arg: List(Exp))
  Let(typ: core.Type, var: String, val: Exp, exp: Exp)
  If(typ: core.Type, cond: Exp, then_exp: Exp, else_exp: Exp)
  Panic(typ: core.Type, e: Exp)
}

pub fn cc_module(mod: mono.Context) {
  let c = CC(Module([], [], []), 1)

  let c =
    dict.fold(mod.out.functions, c, fn(c, _, fun) {
      let #(c, e) = cc(c, fun.body)

      let parameters = list.map(fun.parameters, fn(x) { x.name })
      let function = Function(fun.id, parameters, e, fun.typ.typ)

      let mod = Module(..c.mod, functions: [function, ..c.mod.functions])
      CC(..c, mod: mod)
    })

  let c =
    dict.fold(mod.out.types, c, fn(c, _, custom) {
      let variants =
        list.map(custom.variants, fn(v) {
          let fields =
            list.index_map(v.fields, fn(f, i) {
              Field(gen_names.get_field_name(i), f)
            })
          Variant(v.id, v.display_name, fields)
        })
      let pointer = case variants {
        [v] ->
          case v.fields {
            [] -> True
            _ -> False
          }
        _ -> True
      }
      let custom =
        CustomType(
          name: custom.id,
          display_name: custom.display_name,
          variants: variants,
          pointer: pointer,
        )

      let mod = Module(..c.mod, types: [custom, ..c.mod.types])
      CC(..c, mod: mod)
    })

  let externals =
    mod.out.externals
    |> dict.values()
    |> list.filter(fn(ext) { !ext.mono })
    |> list.map(fn(external) {
      let assert core.FunctionType(param_types, ret) = external.typ.typ
      let params =
        list.index_map(param_types, fn(_, i) { "a" <> int.to_string(i) })
      let todo_val = Literal(core.string_type, core.String("todo"))
      let body = Panic(ret, todo_val)
      Function(
        name: external.id,
        params: params,
        body: body,
        typ: external.typ.typ,
      )
    })

  Module(..c.mod, externals:)
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: core.Exp) -> List(#(String, core.Type)) {
  case e {
    core.Literal(_, _) -> []
    core.Local(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [#(var, typ)]
      }
    }
    core.Global(_, _) -> []
    core.Call(_, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    core.Fn(_, vars, exp) -> {
      let vars = list.map(vars, fn(x) { x.name })
      let n = combine(vars, n)
      fv(n, exp)
    }
    core.Let(_, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, val)
      combine(v, fv(n, exp))
    }
    core.If(_, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
    core.Panic(_, val) -> fv(n, val)
  }
}

fn cc(c: CC, e: core.Exp) -> #(CC, Exp) {
  case e {
    core.Literal(typ, var) -> #(c, Literal(typ, var))
    core.Local(typ, var) -> #(c, Var(typ, var))

    core.Global(core.FunctionType(..) as typ, var) -> {
      let val =
        Call(typ, Var(typ, "create_function"), [
          Var(core.NamedType("void*", []), var),
        ])
      #(c, val)
    }
    core.Global(typ, var) -> {
      #(c, Var(typ, var))
    }
    // detect "direct" function calls
    core.Call(typ, core.Global(fun_type, fun_name), args) -> {
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
    core.Call(typ, fun, args) -> {
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
    core.Fn(typ, vars, exp) -> {
      // TODO recursive call before or after converting this?
      let #(c, exp) = cc(c, exp)

      let var_names = list.map(vars, fn(x) { x.name })
      let var_types = list.map(vars, fn(x) { x.typ })

      let closure_fields = list.unique(fv(var_names, e))

      case closure_fields {
        [] -> {
          // create global function
          let id = int.to_string(c.uid)
          let fun_name = "Closure_" <> id
          let fun = Function(fun_name, var_names, exp, typ)
          let functions = [fun, ..c.mod.functions]
          let c = CC(Module(..c.mod, functions:), c.uid + 1)

          // make a closure reference to the function
          let val =
            Call(typ, Var(typ, "create_function"), [
              Var(core.NamedType("void*", []), fun_name),
            ])
          #(c, val)
        }
        _ -> {
          // create global function
          let id = int.to_string(c.uid)
          let fun_name = "Closure_" <> id
          let env_type_name = "ClosureEnv_" <> id
          let env_type = core.NamedType(env_type_name, [])

          let fun_params = ["ENV", ..var_names]

          // add let bindings to unpack the closure
          let fun_body =
            closure_fields
            |> list.index_fold(exp, fn(exp, field, i) {
              let #(name, typ) = field

              // function from env -> field
              let extract_fun_name = gen_names.get_getter_name(env_type_name, i)
              let extract_fun_type = core.FunctionType([env_type], typ)
              let extract_fun = Var(extract_fun_type, extract_fun_name)

              Let(
                exp.typ,
                name,
                Call(typ, extract_fun, [Var(env_type, "ENV")]),
                exp,
              )
            })

          // add cloure function to module
          let fun_type =
            core.FunctionType([env_type, ..var_types], fun_body.typ)
          let fun = Function(fun_name, fun_params, fun_body, fun_type)
          let functions = [fun, ..c.mod.functions]

          // create the closure object
          let fun_pointer = Var(core.NamedType("void*", []), fun_name)

          let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
          let new_env_fun_type = core.FunctionType(env_arg_types, env_type)
          let env_args = list.map(closure_fields, fn(x) { Var(x.1, x.0) })

          let env_constructor_call =
            Call(
              env_type,
              Var(new_env_fun_type, "new_" <> env_type_name),
              env_args,
            )

          let closure =
            Call(typ, Var(typ, "create_closure"), [
              fun_pointer,
              env_constructor_call,
            ])

          let fields = list.map(closure_fields, fn(x) { Field(x.0, x.1) })
          let variant = Variant(env_type_name, "$", fields)
          let typedef = CustomType(env_type_name, "$", [variant], True)

          let types = [typedef, ..c.mod.types]

          let c = CC(Module(..c.mod, types:, functions:), c.uid + 1)

          #(c, closure)
        }
      }
    }
    core.Let(typ, var, val, exp) -> {
      let #(c, val) = cc(c, val)
      let #(c, exp) = cc(c, exp)
      #(c, Let(typ, var, val, exp))
    }
    core.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, cond)
      let #(c, then_e) = cc(c, then_e)
      let #(c, else_e) = cc(c, else_e)
      #(c, If(typ, cond, then_e, else_e))
    }
    core.Panic(typ, val) -> {
      let #(c, val) = cc(c, val)
      #(c, Panic(typ, val))
    }
  }
}
