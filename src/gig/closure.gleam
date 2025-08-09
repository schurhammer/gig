import gig/core
import gig/mono

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
    untyped_name: String,
    display_name: String,
    variants: List(Variant),
    pointer: Bool,
  )
}

pub type Variant {
  Variant(
    name: String,
    untyped_name: String,
    display_name: String,
    fields: List(Field),
  )
}

pub type Field {
  Field(name: String, typ: mono.Type)
}

pub type Function {
  Function(name: String, parameters: List(Field), return: mono.Type, body: Exp)
}

pub type Exp {
  Literal(typ: mono.Type, val: core.LiteralKind)
  Var(typ: mono.Type, var: String)
  Call(typ: mono.Type, fun: Exp, arg: List(Exp))
  Op(typ: mono.Type, op: mono.Op, arg: List(Exp))
  CallClosure(typ: mono.Type, fun: Exp, arg: List(Exp))
  Let(typ: mono.Type, var: String, val: Exp, exp: Exp)
  If(typ: mono.Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub fn cc_module(mod: mono.Context) {
  let c = CC(Module([], [], []), 1)

  let c =
    list.fold(mod.functions, c, fn(c, fun) {
      let #(c, e) = cc(c, fun.body)

      let parameters = list.map(fun.parameters, fn(x) { Field(x.name, x.typ) })

      let function = Function(fun.name, parameters, fun.return, e)

      let mod = Module(..c.mod, functions: [function, ..c.mod.functions])
      CC(..c, mod: mod)
    })

  let c =
    list.fold(mod.types, c, fn(c, custom) {
      let variants =
        list.map(custom.variants, fn(v) {
          let fields = list.map(v.fields, fn(f) { Field(f.name, f.typ) })
          Variant(v.name, v.untyped_name, v.display_name, fields)
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
          name: custom.name,
          untyped_name: custom.untyped_name,
          display_name: custom.display_name,
          variants: variants,
          pointer: pointer,
        )

      let mod = Module(..c.mod, types: [custom, ..c.mod.types])
      CC(..c, mod: mod)
    })

  let externals =
    mod.externals
    |> list.filter(fn(ext) { !ext.mono })
    |> list.map(fn(external) {
      let assert mono.FunctionType(_, ret) = external.typ
      let params =
        list.map(external.parameters, fn(param) { Field(param.name, param.typ) })
      let string_type = mono.NamedType("String", [])
      let todo_val = Literal(string_type, core.String(""))
      let body = Op(ret, mono.Panic, [todo_val])
      Function(
        name: external.external_name,
        parameters: params,
        return: ret,
        body: body,
      )
    })

  Module(..c.mod, externals:)
}

fn combine(a: List(a), b: List(a)) -> List(a) {
  list.fold(a, b, fn(b, a) { [a, ..b] })
}

fn fv(n: List(String), e: mono.Exp) -> List(#(String, mono.Type)) {
  case e {
    mono.Literal(_, _) -> []
    mono.Local(typ, var) -> {
      case list.contains(n, var) {
        True -> []
        False -> [#(var, typ)]
      }
    }
    mono.Global(_, _) -> []
    mono.Call(_, fun, args) -> {
      let v = fv(n, fun)
      list.fold(args, v, fn(v, arg) { combine(fv(n, arg), v) })
    }
    mono.Op(_, _, args) -> {
      list.fold(args, [], fn(v, arg) { combine(fv(n, arg), v) })
    }
    mono.Fn(_, vars, exp) -> {
      let vars = list.map(vars, fn(x) { x.name })
      let n = combine(vars, n)
      fv(n, exp)
    }
    mono.Let(_, var, val, exp) -> {
      let n = [var, ..n]
      let v = fv(n, val)
      combine(v, fv(n, exp))
    }
    mono.If(_, cond, then_e, else_e) -> {
      let v = fv(n, cond)
      let v = combine(v, fv(n, then_e))
      combine(v, fv(n, else_e))
    }
  }
}

fn cc(c: CC, e: mono.Exp) -> #(CC, Exp) {
  case e {
    mono.Literal(typ, var) -> #(c, Literal(typ, var))
    mono.Local(typ, var) -> #(c, Var(typ, var))

    mono.Global(mono.FunctionType(..) as typ, var) -> {
      let val =
        Call(typ, Var(typ, "create_function"), [
          Var(mono.NamedType("void*", []), var),
        ])
      #(c, val)
    }
    mono.Global(typ, var) -> {
      #(c, Var(typ, var))
    }
    // detect "direct" function calls
    mono.Call(typ, mono.Global(fun_type, fun_name), args) -> {
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
    mono.Call(typ, fun, args) -> {
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
    mono.Op(typ, op, args) -> {
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, arg) {
          let #(c, args) = acc
          let #(c, arg) = cc(c, arg)
          #(c, [arg, ..args])
        })
      let args = list.reverse(args)
      #(c, Op(typ, op, args))
    }
    mono.Fn(typ, vars, exp) -> {
      let #(c, body) = cc(c, exp)

      let var_names = list.map(vars, fn(x) { x.name })
      let var_types = list.map(vars, fn(x) { x.typ })

      let closure_fields = list.unique(fv(var_names, e))

      case closure_fields {
        [] -> {
          // create global function
          let id = int.to_string(c.uid)
          let name = "Closure_" <> id

          let parameters =
            list.zip(var_names, var_types)
            |> list.map(fn(i) { Field(i.0, i.1) })

          let return = body.typ

          let fun = Function(name:, parameters:, return:, body:)
          let functions = [fun, ..c.mod.functions]
          let c = CC(Module(..c.mod, functions:), c.uid + 1)

          // make a closure reference to the function
          let val =
            Call(typ, Var(typ, "create_function"), [
              Var(mono.NamedType("void*", []), name),
            ])
          #(c, val)
        }
        _ -> {
          // create global function
          let id = int.to_string(c.uid)
          let name = "Closure_" <> id
          let env_type_name = "ClosureEnv_" <> id
          let env_type = mono.NamedType(env_type_name, [])

          let var_names = ["ENV", ..var_names]
          let var_types = [env_type, ..var_types]
          let parameters =
            list.zip(var_names, var_types)
            |> list.map(fn(i) { Field(i.0, i.1) })

          let return = body.typ

          // add let bindings to unpack the closure
          let body =
            closure_fields
            |> list.fold(body, fn(exp, field) {
              let #(name, typ) = field

              // function from env -> field
              let access =
                Op(
                  typ,
                  mono.FieldAccess(
                    mono.StructPointerAccess,
                    env_type_name,
                    name,
                  ),
                  [
                    Var(env_type, "ENV"),
                  ],
                )

              Let(exp.typ, name, access, exp)
            })

          let fun = Function(name:, parameters:, return:, body:)
          let functions = [fun, ..c.mod.functions]

          // create the closure object
          let fun_pointer = Var(mono.NamedType("void*", []), name)

          let env_arg_types = closure_fields |> list.map(fn(x) { x.1 })
          let new_env_fun_type = mono.FunctionType(env_arg_types, env_type)
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
          let variant =
            Variant(env_type_name, env_type_name, env_type_name, fields)
          let typedef =
            CustomType(
              env_type_name,
              env_type_name,
              env_type_name,
              [variant],
              True,
            )

          let types = [typedef, ..c.mod.types]

          let c = CC(Module(..c.mod, types:, functions:), c.uid + 1)

          #(c, closure)
        }
      }
    }
    mono.Let(typ, var, val, exp) -> {
      let #(c, val) = cc(c, val)
      let #(c, exp) = cc(c, exp)
      #(c, Let(typ, var, val, exp))
    }
    mono.If(typ, cond, then_e, else_e) -> {
      let #(c, cond) = cc(c, cond)
      let #(c, then_e) = cc(c, then_e)
      let #(c, else_e) = cc(c, else_e)
      #(c, If(typ, cond, then_e, else_e))
    }
  }
}
