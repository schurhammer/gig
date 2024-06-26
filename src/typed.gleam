import call_graph
import env.{type Env}
import graph

import glance as g

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type TypeVarRef {
  TypeVarRef(id: Int)
}

pub type Function {
  Function(name: #(String, String), typ: Poly, params: List(Param), body: Exp)
}

pub type Param {
  Param(label: String, name: String, typ: Type)
}

pub type Constructor {
  Constructor(name: String, typ: Poly, variant: Variant, custom: CustomType)
}

pub type Getter {
  Getter(
    name: String,
    typ: Poly,
    variant: Variant,
    field: Field,
    custom: CustomType,
  )
}

pub type CustomType {
  CustomType(
    name: #(String, String),
    params: List(String),
    variants: List(Variant),
    typ: Poly,
  )
}

pub type Variant {
  Variant(name: #(String, String), fields: List(Field))
}

pub type Field {
  Field(name: String, typ: Type)
}

pub type GlobalVar {
  FunctionVar(poly: Poly)
  BuiltInVar(poly: Poly, external_name: #(String, String))
  BuiltInPolyVar(poly: Poly)
  ConstructorVar(poly: Poly, variant: Variant, custom: CustomType)
  IsaVar(poly: Poly, variant: Variant, custom: CustomType)
  GetterVar(poly: Poly, field: Field, variant: Variant, custom: CustomType)
}

pub type LiteralKind {
  Int(value: String)
  Float(value: String)
  String(value: String)
}

pub type Exp {
  Literal(typ: Type, value: LiteralKind)
  Var(typ: Type, var: String)
  GlobalVar(typ: Type, var: #(String, String))
  Call(typ: Type, fun: Exp, args: List(Exp))
  Fn(typ: Type, var: List(String), exp: Exp)
  // TODO The way bindings work in the compiler limits what actually ends up
  // in the `val` field. Maybe we should reflect this in the type.
  Let(typ: Type, var: String, val: Exp, exp: Exp)
  If(typ: Type, cond: Exp, then_exp: Exp, else_exp: Exp)
}

pub type Poly {
  // TODO should vars be List(TypeVarRef) ??
  Poly(vars: List(Int), typ: Type)
}

pub type Type {
  TypeVar(var: TypeVarRef)
  TypeApp(typ: #(String, String), args: List(Type))
  TypeFun(ret: Type, args: List(Type))
}

pub type TypeVar {
  Bound(Type)
  Unbound(id: Int, level: Int)
}

pub type LocalEnv =
  Env(String, Type)

pub type TypeVarEnv =
  Env(TypeVarRef, TypeVar)

pub type Context {
  Context(
    name: String,
    type_vars: TypeVarEnv,
    types: List(CustomType),
    functions: List(Function),
    // TODO can/should global env replace the functions and types list or vice versa?
    module_names: Env(String, String),
    type_env: Env(#(String, String), CustomType),
    global_env: Env(#(String, String), GlobalVar),
    type_uid: Int,
    temp_uid: Int,
    // TODO I think we technically only need two levels
    // "top level" or "local level"
    // since we only generalise top level functions (citation needed)
    level: Int,
  )
}

pub const builtin = ""

const int = TypeApp(#(builtin, "Int"), [])

const float = TypeApp(#(builtin, "Float"), [])

const bool = TypeApp(#(builtin, "Bool"), [])

const string = TypeApp(#(builtin, "String"), [])

const bool_binop = TypeFun(bool, [bool, bool])

const bool_uop = TypeFun(bool, [bool])

const panic_ast = g.Call(g.Variable("panic_exit"), [])

const true_constructor = GlobalVar(bool, #(builtin, "True"))

fn get_id(a: Type) -> Int {
  let assert TypeVar(a) = a
  a.id
}

// add to global env in the current module
fn put_global_env(c: Context, name: String, kind: GlobalVar) -> Context {
  Context(..c, global_env: env.put(c.global_env, #(c.name, name), kind))
}

// look up a global name where the module might be aliased
fn resolve_alias(c: Context, name: #(String, String)) {
  let name = case env.get(c.module_names, name.0) {
    Ok(mod) -> #(mod, name.1)
    _ -> name
  }
  case env.get(c.global_env, name) {
    Ok(var) -> Ok(#(name, var))
    Error(_) -> Error(Nil)
  }
}

fn register_custom_type(c: Context, custom: CustomType) -> Context {
  let c = Context(..c, type_env: env.put(c.type_env, custom.name, custom))
  let c =
    list.fold(custom.variants, c, fn(c, v) {
      // register constructor function
      let n = c.global_env
      let vars = custom.typ.vars
      let custom_typ = custom.typ.typ
      let args = list.map(v.fields, fn(f) { f.typ })

      let typ = Poly(vars, TypeFun(custom_typ, args))
      let kind = ConstructorVar(typ, v, custom)

      let n = env.put(n, v.name, kind)

      // register isa function
      let typ = Poly(vars, TypeFun(bool, [custom_typ]))
      let kind = IsaVar(typ, v, custom)
      let n = env.put(n, #(v.name.0, "isa_" <> v.name.1), kind)

      // register getters for each field
      let n =
        list.fold(v.fields, n, fn(n, f) {
          let getter_name = v.name.1 <> "_" <> f.name
          let typ = Poly(vars, TypeFun(f.typ, [custom_typ]))
          let kind = GetterVar(typ, f, v, custom)
          env.put(n, #(v.name.0, getter_name), kind)
        })

      Context(..c, global_env: n)
    })

  Context(..c, types: [custom, ..c.types])
}

pub fn prelude_context() {
  let c =
    Context(
      name: builtin,
      type_vars: env.new(),
      types: [],
      functions: [],
      module_names: env.new(),
      type_env: env.new(),
      global_env: env.new(),
      type_uid: 1000,
      temp_uid: 0,
      level: 0,
    )

  let put_builtin = fn(c, name, typ) {
    put_global_env(c, name, BuiltInVar(typ, #(builtin, name)))
  }

  // let c = put_builtin(c, "Nil", Poly([], nil))
  // let c = put_builtin(c, "isa_Nil", Poly([], TypeFun(bool, [nil])))

  let c = put_builtin(c, "True", Poly([], bool))
  let c = put_builtin(c, "False", Poly([], bool))
  let c = put_builtin(c, "isa_True", Poly([], bool_uop))
  let c = put_builtin(c, "isa_False", Poly([], bool_uop))

  let #(c, a) = new_type_var_ref(c)
  let equal_type = Poly([get_id(a)], TypeFun(bool, [a, a]))
  let c = put_global_env(c, "equal", BuiltInPolyVar(equal_type))

  let #(c, a) = new_type_var_ref(c)
  let inspect_type = Poly([get_id(a)], TypeFun(string, [a]))
  let c = put_global_env(c, "gleam_inspect", BuiltInPolyVar(inspect_type))

  c
}

pub fn infer_module(c: Context, mod: g.Module) {
  // add dummy types so they can reference eachother
  let c =
    list.fold(mod.custom_types, c, fn(c, def) {
      let ct = def.definition
      let dummy = Poly([], TypeApp(#(builtin, "Dummy"), []))
      let dummy = CustomType(#(c.name, ct.name), [], [], dummy)
      Context(..c, type_env: env.put(c.type_env, #(c.name, ct.name), dummy))
    })
  // now add types fr
  let c =
    list.fold(mod.custom_types, c, fn(c, def) {
      let #(c, custom) = infer_custom_type(c, def.definition)
      register_custom_type(c, custom)
    })

  // functions

  let rec_groups =
    call_graph.create(mod)
    |> graph.strongly_connected_components()

  let c =
    list.fold(rec_groups, c, fn(c, group) {
      // TODO can I start a typevars environment for each group? might be faster

      // enter let level
      let c = enter_level(c)

      // find the function definitions by name
      let group =
        list.map(group, fn(fun_name) {
          let assert Ok(def) =
            list.find(mod.functions, fn(x) {
              let fun = x.definition
              fun.name == fun_name
            })
          def
        })

      // find external functions
      let #(ext, group) =
        list.partition(group, fn(def) {
          list.any(def.attributes, fn(att) {
            case att {
              g.Attribute("external", _) -> True
              _ -> False
            }
          })
        })

      // process external functions
      let c =
        list.fold(ext, c, fn(c, def) {
          let fun = def.definition
          let assert Ok(attr) =
            list.find(def.attributes, fn(att) {
              case att {
                g.Attribute("external", _) -> True
                _ -> False
              }
            })
          let assert [_, g.String(ext_mod), g.String(ext_name)] = attr.arguments
          let #(c, params, ret) = function_parameters(c, fun)
          let params = list.map(params, fn(x) { x.typ })

          let c = exit_level(c)
          let gen = generalise(c, TypeFun(ret, params))
          let c = enter_level(c)

          let var = BuiltInVar(gen, #(ext_mod, ext_name))
          put_global_env(c, fun.name, var)
        })

      // put the functions into env with placeholder types
      let c =
        list.fold(group, c, fn(c, fun) {
          let #(c, typ) = new_type_var_ref(c)
          let kind = FunctionVar(Poly([], typ))
          put_global_env(c, fun.definition.name, kind)
        })

      // TODO better way to access function params while inferring
      // insert dummy functions in the module
      // used for accessing parameter labels
      let funs = c.functions
      let n = env.new()
      let #(c, group) =
        list.fold(group, #(c, []), fn(acc, def) {
          let #(c, l) = acc
          let fun = def.definition
          let #(c, params, ret) = function_parameters(c, fun)
          let dummy_exp = true_constructor
          let fun =
            Function(#(c.name, fun.name), Poly([], bool), params, dummy_exp)
          let c = Context(..c, functions: [fun, ..c.functions])
          #(c, [#(def, params), ..l])
        })

      // infer each function
      let #(c, group) =
        list.fold(group, #(c, []), fn(acc, x) {
          let #(c, group) = acc
          let #(def, params) = x

          // infer the function
          let #(c, fun) = infer_function(c, def.definition, params)

          #(c, [fun, ..group])
        })

      // exit let level
      let c = exit_level(c)

      // revert dummy functions we added earlier
      let c = Context(..c, functions: funs)

      // generalise functions
      let #(c, group) =
        list.fold(group, #(c, []), fn(acc, fun) {
          let #(c, group) = acc

          let Function(name, gen, params, body) = fun

          // generalise the type
          let gen = generalise(c, gen.typ)

          // update the env with generalised type
          let fun = Function(name, gen, params, body)
          let kind = FunctionVar(gen)
          let c = put_global_env(c, name.1, kind)
          #(c, [fun, ..group])
        })

      Context(..c, functions: list.append(group, c.functions))
    })

  unshadow_context(c)
}

pub fn generalise(c: Context, typ: Type) {
  let tvs =
    list.unique(find_tvs(c, typ))
    |> list.sort(int.compare)
  Poly(tvs, typ)
}

pub fn get_type_var(c: Context, var: TypeVarRef) {
  let assert Ok(x) = env.get(c.type_vars, var)
  x
}

pub fn set_type_var(c: Context, var: TypeVarRef, bind: TypeVar) {
  Context(..c, type_vars: env.put(c.type_vars, var, bind))
}

fn new_type_var_ref(c: Context) -> #(Context, Type) {
  let ref = TypeVarRef(c.type_uid)
  let type_vars = env.put(c.type_vars, ref, Unbound(c.type_uid, c.level))
  #(Context(..c, type_vars: type_vars, type_uid: c.type_uid + 1), TypeVar(ref))
}

fn new_temp_var(c: Context) -> #(Context, String) {
  let id = "T" <> int.to_string(c.temp_uid)
  #(Context(..c, temp_uid: c.temp_uid + 1), id)
}

fn infer_custom_type(c: Context, ct: g.CustomType) {
  // create a type variable for each parameter
  // these will be used when a field references a type parameter
  let #(c, param_types) =
    list.fold(ct.parameters, #(c, []), fn(acc, p) {
      let #(c, l) = acc
      let #(c, typ) = new_type_var_ref(c)
      let assert TypeVar(ref) = typ
      #(c, [#(p, typ, ref.id), ..l])
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
  let typ = Poly(type_params, TypeApp(#(c.name, ct.name), type_args))

  #(c, CustomType(#(c.name, ct.name), ct.parameters, variants, typ))
}

fn infer_variant(c: Context, n: Env(String, Type), variant: g.Variant) {
  let #(c, fields) =
    list.index_fold(variant.fields, #(c, []), fn(acc, field, i) {
      let #(c, l) = acc
      let #(c, t) = infer_type(c, n, field.item)
      let name = case field.label {
        Some(x) -> x
        None -> "_field_" <> int.to_string(i)
      }
      #(c, [Field(name, t), ..l])
    })
  let fields = list.reverse(fields)

  #(c, Variant(#(c.name, variant.name), fields))
}

fn resolve_type_name(
  c: Context,
  mod: Option(String),
  name: String,
) -> #(String, String) {
  case mod {
    Some(mod) -> {
      let qual_name = case env.get(c.module_names, mod) {
        Ok(mod) -> #(mod, name)
        _ -> #(mod, name)
      }
      case env.get(c.type_env, qual_name) {
        Ok(custom) -> custom.name
        _ -> panic as { "could not resolve type " <> mod <> "." <> name }
      }
    }
    None ->
      // check this module
      case env.get(c.type_env, #(c.name, name)) {
        Ok(typ) -> typ.name
        _ ->
          // check prelude
          case env.get(c.type_env, #(builtin, name)) {
            Ok(typ) -> typ.name
            _ -> panic as { "could not resolve type " <> name }
          }
      }
  }
}

fn infer_type(c: Context, n: Env(String, Type), typ: g.Type) -> #(Context, Type) {
  case typ {
    g.NamedType(name, module, params) -> {
      let #(c, params) =
        list.fold(params, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = infer_type(c, n, p)
          #(c, [p, ..l])
        })
      let params = list.reverse(params)
      let name = resolve_type_name(c, module, name)
      #(c, TypeApp(name, params))
    }
    g.TupleType(elements) -> {
      let size = list.length(elements)
      let #(c, type_name) = register_tuple(c, size)
      let typ = g.NamedType(type_name, None, elements)
      infer_type(c, n, typ)
    }
    g.FunctionType(parameters, return) -> {
      let #(c, params) =
        list.fold(parameters, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = infer_type(c, n, p)
          #(c, [p, ..l])
        })
      let params = list.reverse(params)
      let #(c, ret) = infer_type(c, n, return)
      #(c, TypeFun(ret, params))
    }
    g.VariableType(name) -> {
      let assert Ok(t) = env.get(n, name)
      #(c, t)
    }
    g.HoleType(name) -> todo
  }
}

type ResolvedAccess {
  FieldAccess(variant: Variant, field: Field, typ: Type)
  ModuleAccess(name: #(String, String), typ: GlobalVar)
}

fn find_custom_type(
  c: Context,
  name: #(String, String),
) -> Result(CustomType, Nil) {
  list.find(c.types, fn(x) { x.name == name })
}

fn find_field(variant: Variant, name: String) -> Result(Field, Nil) {
  list.find(variant.fields, fn(x) { x.name == name })
}

/// Find a name from the local env or global env. Instantiate if needed.
fn resolve_access(
  c: Context,
  n: LocalEnv,
  qualifier: String,
  name: String,
) -> Result(ResolvedAccess, String) {
  let res = resolve_name(c, n, qualifier)
  let field_access = case res {
    Ok(LocalName(_local, typ)) -> {
      case resolve_type(c, typ) {
        TypeApp(custom_name, _typ) ->
          case find_custom_type(c, custom_name) {
            Ok(custom) ->
              case custom.variants {
                [variant] ->
                  case find_field(variant, name) {
                    Ok(field) -> Ok(FieldAccess(variant, field, field.typ))
                    _ -> Error(Nil)
                  }
                _ -> Error(Nil)
              }
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
  case field_access {
    Ok(field_access) -> Ok(field_access)
    // not a field access, try module access
    _ ->
      case resolve_alias(c, #(qualifier, name)) {
        Ok(#(name, var)) -> Ok(ModuleAccess(name, var))
        _ -> Error("could not resolve name " <> qualifier <> "." <> name)
      }
  }
}

type ResolvedName {
  LocalName(name: String, typ: Type)
  GlobalName(name: #(String, String), typ: GlobalVar)
}

fn resolve_name(
  c: Context,
  n: LocalEnv,
  name: String,
) -> Result(ResolvedName, String) {
  case env.get(n, name) {
    // try local env
    Ok(typ) -> Ok(LocalName(name, typ))
    _ ->
      // try global env
      case resolve_alias(c, #(c.name, name)) {
        Ok(#(name, typ)) -> Ok(GlobalName(name, typ))
        // try prelude
        _ ->
          case resolve_alias(c, #(builtin, name)) {
            Ok(#(name, typ)) -> Ok(GlobalName(name, typ))
            _ -> Error("could not resolve name " <> name)
          }
      }
  }
}

fn find_type_vars(t: g.Type) -> List(String) {
  case t {
    g.NamedType(name, module, parameters) ->
      list.flat_map(parameters, find_type_vars)
    g.TupleType(elements) -> list.flat_map(elements, find_type_vars)
    g.FunctionType(parameters, return) ->
      list.flat_map([return, ..parameters], find_type_vars)
    g.VariableType(name) -> [name]
    g.HoleType(name) -> todo
  }
}

fn function_parameters(c: Context, fun: g.Function) {
  // find type variables used in the functions type annotation
  let vars =
    list.flat_map(fun.parameters, fn(param) {
      case param.type_ {
        Some(typ) -> find_type_vars(typ)
        None -> []
      }
    })
  let vars = case fun.return {
    Some(ret) -> list.append(find_type_vars(ret), vars)
    None -> vars
  }
  let vars =
    vars
    |> list.unique()
    |> list.sort(string.compare)

  // create an env for the type variables
  let #(c, type_env) =
    list.fold(vars, #(c, env.new()), fn(acc, x) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = env.put(n, x, typ)
      #(c, n)
    })

  // create type vars for parameters
  let #(c, params) =
    list.index_fold(fun.parameters, #(c, []), fn(acc, param, i) {
      let #(c, param_types) = acc

      let name = case param.name {
        g.Named(s) -> s
        g.Discarded(s) -> s
      }

      let label = case param.label {
        Some(x) -> x
        _ -> "_param_" <> int.to_string(i)
      }

      let #(c, typ) = case param.type_ {
        Some(typ) -> infer_type(c, type_env, typ)
        None -> new_type_var_ref(c)
      }

      #(c, [Param(label, name, typ), ..param_types])
    })
  let params = list.reverse(params)

  let #(c, ret) = case fun.return {
    Some(ret) -> infer_type(c, type_env, ret)
    None -> new_type_var_ref(c)
  }

  #(c, params, ret)
}

fn infer_function(
  c: Context,
  fun: g.Function,
  params: List(Param),
) -> #(Context, Function) {
  // put params into env
  let n =
    list.fold(params, env.new(), fn(n, param) {
      env.put(n, param.name, param.typ)
    })

  // infer body
  let #(c, body) = infer_body(c, n, fun.body)

  // compute function type
  let types = list.map(params, fn(x) { x.typ })
  let typ = TypeFun(body.typ, types)

  // TODO access poly without env.get (pass in?)
  // TODO this unify (and type variable) is only needed for recursive functions
  let assert Ok(#(_name, var)) = resolve_alias(c, #(c.name, fun.name))
  let c = unify(c, typ, var.poly.typ)

  // this is not yet the final function definition
  // the final version will be generalised after the recursive group is finished
  let fun = Function(#(c.name, fun.name), var.poly, params, body)

  #(c, fun)
}

fn enter_level(c: Context) -> Context {
  Context(..c, level: c.level + 1)
}

fn exit_level(c: Context) -> Context {
  Context(..c, level: c.level - 1)
}

fn infer_body(
  c: Context,
  n: LocalEnv,
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

          let #(c, subject_name) = new_temp_var(c)
          let subject = g.Variable(subject_name)
          let n = env.put(n, subject_name, value.typ)
          let #(c, bindings) = infer_bind_pattern(c, n, pattern, subject)

          // add bindings to environment
          let n =
            list.fold(bindings, n, fn(n, binding) {
              let #(name, value) = binding
              env.put(n, name, value.typ)
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
  n: LocalEnv,
  pattern: g.Pattern,
  subject: g.Expression,
) -> #(Context, List(#(String, Exp))) {
  // TODO enter_level here? or do we not need to because gleam doesn't do let-poly
  // I think its done in infer_body already
  case pattern {
    g.PatternInt(_) -> #(c, [])
    g.PatternFloat(_) -> #(c, [])
    g.PatternDiscard(_) -> #(c, [])
    g.PatternVariable(name) -> {
      let #(c, subject) = infer_expression(c, n, subject)
      #(c, [#(name, subject)])
    }
    g.PatternAssignment(pattern, name) -> {
      let #(c, value) = infer_expression(c, n, subject)
      let #(c, rest) = infer_bind_pattern(c, n, pattern, subject)
      #(c, [#(name, value), ..rest])
    }
    g.PatternTuple(args) -> {
      let size = list.length(args)
      let type_name = "Tuple" <> int.to_string(size)
      let args = list.map(args, fn(x) { g.Field(None, x) })
      let pattern = g.PatternConstructor(Some(builtin), type_name, args, False)
      infer_bind_pattern(c, n, pattern, subject)
    }
    g.PatternList(elements, tail) -> {
      let tail = case tail {
        Some(tail) -> tail
        None -> g.PatternConstructor(Some(builtin), "Empty", [], False)
      }
      let elements = list.reverse(elements)
      let list =
        list.fold(elements, tail, fn(rest, x) {
          let args = [g.Field(None, x), g.Field(None, rest)]
          g.PatternConstructor(Some(builtin), "Cons", args, False)
        })
      infer_bind_pattern(c, n, list, subject)
    }
    g.PatternConstructor(mod, cons, args, _spread) -> {
      let #(_name, kind) = case mod {
        Some(mod) ->
          case resolve_alias(c, #(mod, cons)) {
            Ok(res) -> res
            _ -> panic as { "could not resolve " <> mod <> "." <> cons }
          }
        None ->
          case resolve_name(c, n, cons) {
            Ok(GlobalName(name, var)) -> #(name, var)
            _ -> panic as { "could not resolve " <> cons }
          }
      }

      case kind {
        ConstructorVar(_, variant, _) -> {
          // match labels
          let args = list.index_map(args, fn(x, i) { #(x, i) })
          let args =
            list.index_map(variant.fields, fn(field, i) {
              let assert Ok(x) =
                list.find(args, fn(x) {
                  let #(arg, j) = x
                  case arg.label {
                    Some(some) -> field.name == some
                    None -> i == j
                  }
                })
              x.0
            })
          let assert Ok(args) = list.strict_zip(variant.fields, args)
          // let binding for each item
          let #(c, args) =
            list.fold(args, #(c, []), fn(acc, x) {
              let #(c, args) = acc
              let #(field, pattern) = x
              let getter_name = variant.name.1 <> "_" <> field.name
              let getter_args = [g.Field(None, subject)]
              let subject = g.Call(g.Variable(getter_name), getter_args)
              let #(c, arg) = infer_bind_pattern(c, n, pattern.item, subject)
              #(c, [arg, ..args])
            })
          #(c, list.flatten(args))
        }
        BuiltInVar(_, _) -> #(c, [])
        _ -> {
          io.debug(pattern)
          panic as "expected a constructor"
        }
      }
    }
    _ -> {
      io.debug(pattern)
      todo as "not implemented"
    }
  }
}

fn infer_check_pattern(
  c: Context,
  n: LocalEnv,
  pattern: g.Pattern,
  subject: g.Expression,
) -> #(Context, Exp) {
  case pattern {
    g.PatternInt(value) -> {
      let args = [g.Field(None, g.Int(value)), g.Field(None, subject)]
      infer_expression(c, n, g.Call(g.Variable("equal"), args))
    }
    g.PatternFloat(value) -> {
      let args = [g.Field(None, g.Float(value)), g.Field(None, subject)]
      infer_expression(c, n, g.Call(g.Variable("equal"), args))
    }
    g.PatternDiscard(_) -> #(c, true_constructor)
    g.PatternVariable(_) -> #(c, true_constructor)
    g.PatternAssignment(_, _) -> #(c, true_constructor)
    g.PatternTuple(args) -> {
      let size = list.length(args)
      let type_name = "Tuple" <> int.to_string(size)
      let args = list.map(args, fn(x) { g.Field(None, x) })
      let pattern = g.PatternConstructor(Some(builtin), type_name, args, False)
      infer_check_pattern(c, n, pattern, subject)
    }
    g.PatternConstructor(mod, cons, args, _spread) -> {
      // constructor match
      let #(name, kind) = case mod {
        Some(mod) ->
          case resolve_alias(c, #(mod, cons)) {
            Ok(res) -> res
            _ -> panic as { "could not resolve " <> mod <> "." <> cons }
          }
        None ->
          case resolve_name(c, n, cons) {
            Ok(GlobalName(name, var)) -> #(name, var)
            _ -> panic as { "could not resolve " <> cons }
          }
      }

      let fields = case kind {
        ConstructorVar(_poly, variant, _custom) -> variant.fields
        BuiltInVar(_, _) -> []
        _ -> {
          io.debug(kind)
          panic as "expected a constructor"
        }
      }

      let tag_args = [g.Field(None, subject)]
      let tag = g.FieldAccess(g.Variable(name.0), "isa_" <> cons)
      let #(c, check) = infer_expression(c, n, g.Call(tag, tag_args))

      let assert Ok(args) = list.strict_zip(args, fields)
      // inner match
      let #(c, args) =
        list.fold(args, #(c, []), fn(acc, entry) {
          let #(c, args) = acc
          let #(x, field) = entry
          let getter = g.Variable(cons <> "_" <> field.name)
          let subject = g.Call(getter, [g.Field(None, subject)])
          let #(c, check) = infer_check_pattern(c, n, x.item, subject)
          #(c, [check, ..args])
        })
      let check =
        reduce_right(args, check, fn(checks, check) {
          call_and_bool(check, checks)
        })
      #(c, check)
    }
    g.PatternList(elements, tail) -> {
      let tail = case tail {
        Some(tail) -> tail
        None -> g.PatternConstructor(Some(builtin), "Empty", [], False)
      }
      let elements = list.reverse(elements)
      let list =
        list.fold(elements, tail, fn(rest, x) {
          let args = [g.Field(None, x), g.Field(None, rest)]
          g.PatternConstructor(Some(builtin), "Cons", args, False)
        })
      infer_check_pattern(c, n, list, subject)
    }
    _ -> {
      io.debug(pattern)
      io.debug(subject)
      todo
    }
  }
}

fn reduce_right(list: List(a), item: a, reducer: fn(a, a) -> a) {
  case list {
    [] -> item
    [x, ..xs] -> reducer(reduce_right(xs, x, reducer), item)
  }
}

fn range(a, b) {
  case a < b {
    True -> [a, ..range(a + 1, b)]
    False -> []
  }
}

fn register_tuple(c: Context, size: Int) -> #(Context, String) {
  let type_name = "Tuple" <> int.to_string(size)
  let c = case find_custom_type(c, #(builtin, type_name)) {
    Ok(_) -> c
    Error(_) -> {
      let #(c, fields) =
        list.fold(range(0, size), #(c, []), fn(acc, i) {
          let #(c, l) = acc
          let name = "field" <> int.to_string(i)
          let #(c, typ) = new_type_var_ref(c)
          #(c, [Field(name, typ), ..l])
        })
      let fields = list.reverse(fields)

      let vars =
        list.map(fields, fn(f) {
          let assert TypeVar(ref) = f.typ
          ref.id
        })
      let params = list.map(fields, fn(f) { f.name })
      let type_params = list.map(fields, fn(f) { f.typ })

      let variant = Variant(#(builtin, type_name), fields)
      let poly = Poly(vars, TypeApp(#(builtin, type_name), type_params))
      let custom = CustomType(#(builtin, type_name), params, [variant], poly)

      let c = register_custom_type(c, custom)

      Context(..c, types: [custom, ..c.types])
    }
  }
  #(c, type_name)
}

fn access_builtin(name: String) {
  g.FieldAccess(g.Variable(builtin), name)
}

fn infer_expression(
  c: Context,
  n: LocalEnv,
  exp: g.Expression,
) -> #(Context, Exp) {
  case exp {
    g.Int(s) -> #(c, Literal(int, Int(string.replace(s, "_", ""))))
    g.Float(s) -> #(c, Literal(float, Float(s)))
    g.String(s) -> #(c, Literal(string, String(s)))
    g.Variable(s) -> {
      // instantiate the poly type into a mono type
      let assert Ok(resolved) = resolve_name(c, n, s)
      case resolved {
        // handle 0-arg constructors
        GlobalName(name, ConstructorVar(poly, Variant(_, []), _)) -> {
          let #(c, typ) = instantiate(c, poly)
          let var = GlobalVar(typ, name)

          // new var for the return type
          let #(c, ret) = new_type_var_ref(c)

          // unify the actual function type with the types of args
          let c = unify(c, typ, TypeFun(ret, []))

          #(c, Call(ret, var, []))
        }
        GlobalName(name, var) -> {
          let #(c, typ) = instantiate(c, var.poly)
          #(c, GlobalVar(typ, name))
        }
        LocalName(name, typ) -> {
          #(c, Var(typ, name))
        }
      }
    }
    g.NegateBool(e) -> {
      let fun = g.Call(access_builtin("negate_bool"), [g.Field(None, e)])
      infer_expression(c, n, fun)
    }
    g.NegateInt(e) -> {
      let fun = g.Call(access_builtin("negate_int"), [g.Field(None, e)])
      infer_expression(c, n, fun)
    }
    g.Block(statements) -> infer_body(c, n, statements)
    g.Panic(_) -> infer_expression(c, n, panic_ast)
    g.Todo(_) -> infer_expression(c, n, panic_ast)
    g.Tuple(args) -> {
      let args = list.map(args, fn(x) { g.Field(None, x) })
      let size = list.length(args)
      let #(c, type_name) = register_tuple(c, size)
      infer_expression(c, n, g.Call(access_builtin(type_name), args))
    }
    g.List(elements, tail) -> {
      let tail = case tail {
        Some(tail) -> tail
        None -> access_builtin("Empty")
      }
      let elements = list.reverse(elements)
      let list =
        list.fold(elements, tail, fn(rest, x) {
          let args = [g.Field(None, x), g.Field(None, rest)]
          g.Call(access_builtin("Cons"), args)
        })
      infer_expression(c, n, list)
    }
    g.TupleIndex(tuple, index) -> {
      let field_name = "field" <> int.to_string(index)
      let exp = g.FieldAccess(tuple, field_name)
      infer_expression(c, n, exp)
    }
    g.FnCapture(label, fun, before, after) -> {
      let #(c, x) = new_temp_var(c)
      let arg = g.Field(label, g.Variable(x))
      let args = list.concat([before, [arg], after])
      let param = g.FnParameter(g.Named(x), None)
      let abs = g.Fn([param], None, [g.Expression(g.Call(fun, args))])
      infer_expression(c, n, abs)
    }
    g.Call(fun, args) -> {
      // handle labeled arguments
      let args = case fun {
        g.Variable(name) -> {
          let assert Ok(resolved) = resolve_name(c, n, name)
          case resolved {
            GlobalName(name, FunctionVar(_)) -> {
              // TODO not sure if this algorithm is canon
              // match labels
              let assert Ok(fun) =
                list.find(c.functions, fn(x) { x.name == name })
              let args = list.index_map(args, fn(x, i) { #(x, i) })
              list.index_map(fun.params, fn(param, i) {
                let assert Ok(x) =
                  list.find(args, fn(x) {
                    let #(arg, j) = x
                    case arg.label {
                      Some(some) -> param.label == some
                      None -> i == j
                    }
                  })
                x.0
              })
            }
            GlobalName(name, ConstructorVar(_, variant, _)) -> {
              // match labels
              let args = list.index_map(args, fn(x, i) { #(x, i) })
              list.index_map(variant.fields, fn(field, i) {
                let assert Ok(x) =
                  list.find(args, fn(x) {
                    let #(arg, j) = x
                    case arg.label {
                      Some(some) -> field.name == some
                      None -> i == j
                    }
                  })
                x.0
              })
            }
            _ -> args
          }
        }
        _ -> args
      }

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
      // create type vars for parameters
      let #(c, n, param_names, param_types) =
        list.fold(params, #(c, n, [], []), fn(acc, param) {
          let #(c, n, names, typs) = acc

          let name = case param.name {
            g.Named(s) -> s
            g.Discarded(s) -> "_" <> s
          }

          let #(c, typ) = case param.type_ {
            Some(typ) -> infer_type(c, env.new(), typ)
            None -> new_type_var_ref(c)
          }

          let n = env.put(n, name, typ)
          #(c, n, [name, ..names], [typ, ..typs])
        })
      let param_names = list.reverse(param_names)
      let param_types = list.reverse(param_types)

      // infer body
      let #(c, body) = infer_body(c, n, body)

      // compute function type
      let typ = TypeFun(body.typ, param_types)

      #(c, Fn(typ, param_names, body))
    }
    g.RecordUpdate(module, constructor, record, fields) -> {
      // Resolve the constructor
      let constructor_name = case module {
        Some(mod_name) -> mod_name <> "." <> constructor
        None -> constructor
      }
      let assert Ok(resolved) = resolve_name(c, n, constructor_name)
      let assert GlobalName(_name, ConstructorVar(_poly, variant, _)) = resolved

      // Create a list of all fields, merging new values with the original record
      let all_fields =
        list.map(variant.fields, fn(field) {
          case list.find(fields, fn(f) { f.0 == field.name }) {
            Ok(#(_, new_value)) -> {
              // If a new value is povided, use it
              g.Field(Some(field.name), new_value)
            }
            Error(Nil) -> {
              // If no new value, access the field from the original record
              let field_access = g.FieldAccess(record, field.name)
              g.Field(Some(field.name), field_access)
            }
          }
        })

      // Fold the field inference results
      let #(c, all_fields) =
        list.fold(all_fields, #(c, []), fn(acc, field) {
          let #(c, fields) = acc
          #(c, [field, ..fields])
        })

      // Reverse the fields to maintain the original order
      let all_fields = list.reverse(all_fields)

      // Create a new constructor call with all fields
      let updated_record = g.Call(g.Variable(constructor), all_fields)

      // Infer the type of the updated record
      infer_expression(c, n, updated_record)
    }
    g.BinaryOperator(name, left, right) -> {
      case name {
        g.Pipe -> {
          case right {
            g.Call(fun, args) ->
              infer_expression(c, n, g.Call(fun, [g.Field(None, left), ..args]))
            g.Variable(_name) ->
              infer_expression(c, n, g.Call(right, [g.Field(None, left)]))
            g.FieldAccess(_value, _field) ->
              infer_expression(c, n, g.Call(right, [g.Field(None, left)]))
            _ -> panic as "pipe to unexpected expression"
          }
        }
        g.NotEq -> {
          let eq = g.BinaryOperator(g.Eq, left, right)
          let neq = g.NegateBool(eq)
          infer_expression(c, n, neq)
        }
        _ -> {
          let fun_name = case name {
            // Bool
            g.Eq -> "equal"
            g.And -> "and_bool"
            g.Or -> "or_bool"
            // Int
            g.AddInt -> "add_int"
            g.SubInt -> "sub_int"
            g.MultInt -> "mul_int"
            g.DivInt -> "div_int"
            g.RemainderInt -> "rem_int"
            g.LtInt -> "lt_int"
            g.GtInt -> "gt_int"
            g.LtEqInt -> "lte_int"
            g.GtEqInt -> "gte_int"
            // Float
            g.AddFloat -> "add_float"
            g.SubFloat -> "sub_float"
            g.MultFloat -> "mul_float"
            g.DivFloat -> "div_float"
            g.LtFloat -> "lt_float"
            g.GtFloat -> "gt_float"
            g.LtEqFloat -> "lte_float"
            g.GtEqFloat -> "gte_float"
            // String
            g.Concatenate -> "append_string"
            _ -> todo as { "binop not implemented " <> string.inspect(name) }
          }
          let args = [g.Field(None, left), g.Field(None, right)]
          let fun = g.Call(access_builtin(fun_name), args)
          infer_expression(c, n, fun)
        }
      }
    }
    g.Case(subjects, clauses) -> {
      // unwrap alternative case patterns into multiple clauses
      // we do this because they bind variables differently
      let clauses =
        list.flat_map(clauses, fn(clause: g.Clause) {
          list.map(clause.patterns, fn(patterns) {
            #(patterns, clause.guard, clause.body)
          })
        })

      // need to reverse to make the last one the innermost after the fold
      let clauses = list.reverse(clauses)

      // the innermost branch will panic due to unmatched value
      let #(c, panic_exp) = infer_expression(c, n, panic_ast)

      // prefix for all subjects
      let #(c, subject_prefix) = new_temp_var(c)
      let subject_prefix = subject_prefix <> "_"

      // create let binding for subjects
      let #(c, n, subjects) =
        list.index_fold(subjects, #(c, n, []), fn(acc, subject, i) {
          let #(c, n, l) = acc
          let c = enter_level(c)
          let #(c, value) = infer_expression(c, n, subject)
          let c = exit_level(c)
          let subject_name = subject_prefix <> int.to_string(i)
          let n = env.put(n, subject_name, value.typ)
          #(c, n, [#(subject_name, value), ..l])
        })

      // process each clause in order
      let #(c, e) =
        list.fold(clauses, #(c, panic_exp), fn(acc, clause) {
          let #(c, else_exp) = acc
          let #(patterns, guard, body) = clause

          // find bindings
          let #(c, bindings) =
            list.index_fold(patterns, #(c, []), fn(acc, pattern, i) {
              let #(c, l) = acc
              let subject = g.Variable(subject_prefix <> int.to_string(i))
              let #(c, bindings) = infer_bind_pattern(c, n, pattern, subject)
              #(c, list.append(bindings, l))
            })

          // apply binding to env
          let n =
            list.fold(bindings, n, fn(n, binding) {
              let #(name, val) = binding
              env.put(n, name, val.typ)
            })

          let #(c, guard) = case guard {
            Some(guard) -> {
              // infer guard expression
              let #(c, guard) = infer_expression(c, n, guard)
              // inline bindings in the guard
              let guard =
                list.fold(bindings, guard, fn(guard, binding) {
                  replace_var(binding.0, binding.1, guard)
                })
              #(c, guard)
            }
            None -> #(c, true_constructor)
          }

          // create boolean expression
          let #(c, cond_exp) =
            list.index_fold(patterns, #(c, guard), fn(acc, pattern, i) {
              let #(c, last_e) = acc

              // get the subject
              let subject = g.Variable(subject_prefix <> int.to_string(i))
              // let #(c, subject) = infer_expression(c, n, subject)

              // check that the subject matches the pattern
              let #(c, e) = infer_check_pattern(c, n, pattern, subject)

              // TODO do we need to check that e is Bool?
              #(c, call_and_bool(e, last_e))
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
    g.FieldAccess(g.Variable(name), field) -> {
      let resolved = resolve_access(c, n, name, field)
      case resolved {
        Ok(FieldAccess(variant, field, typ)) -> {
          let getter_name = variant.name.1 <> "_" <> field.name
          let getter_call =
            g.Call(g.Variable(getter_name), [g.Field(None, g.Variable(name))])
          let #(c, exp) = infer_expression(c, n, getter_call)
          #(c, exp)
        }
        // handle 0-arg constructors
        Ok(ModuleAccess(name, ConstructorVar(poly, Variant(_, []), _))) -> {
          let #(c, typ) = instantiate(c, poly)
          let var = GlobalVar(typ, name)

          // new var for the return type
          let #(c, ret) = new_type_var_ref(c)

          // unify the actual function type with the types of args
          let c = unify(c, typ, TypeFun(ret, []))

          #(c, Call(ret, var, []))
        }
        Ok(ModuleAccess(name, var)) -> {
          let #(c, typ) = instantiate(c, var.poly)
          #(c, GlobalVar(typ, name))
        }
        _ -> panic as { "could not resolve name " <> name <> "." <> field }
      }
    }
    g.FieldAccess(value, field) -> {
      let #(c, exp) = infer_expression(c, n, value)
      let assert TypeApp(type_name, _) = resolve_type(c, exp.typ)

      // accessor only works with exactly one variant
      let assert Ok(custom) = list.find(c.types, fn(x) { x.name == type_name })
      let assert [variant] = custom.variants

      let getter_name = variant.name.1 <> "_" <> field

      let getter_call = g.Call(g.Variable(getter_name), [g.Field(None, value)])
      let #(c, exp) = infer_expression(c, n, getter_call)
      #(c, exp)
    }
    exp -> {
      io.debug(exp)
      todo as "not implemented"
    }
  }
}

/// follow any references to get the real type
pub fn resolve_type(c: Context, typ: Type) {
  case typ {
    TypeVar(x) -> {
      let assert Ok(x) = env.get(c.type_vars, x)
      case x {
        Bound(x) -> resolve_type(c, x)
        Unbound(..) -> typ
      }
    }
    TypeApp(..) -> typ
    TypeFun(..) -> typ
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

fn call_and_bool(a, b) {
  let and_bool = GlobalVar(bool_binop, #(builtin, "and_bool"))

  case a, b {
    GlobalVar(_, #(builtin, "True")), _ -> b
    _, GlobalVar(_, #(builtin, "True")) -> a
    _, _ -> Call(bool, and_bool, [a, b])
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

pub fn instantiate(c: Context, poly: Poly) -> #(Context, Type) {
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

fn unshadow(taken: List(String), i: Int, e: Exp) -> #(List(String), Int, Exp) {
  case e {
    Literal(_, _) -> #(taken, i, e)
    Var(_, _) -> #(taken, i, e)
    GlobalVar(_, _) -> #(taken, i, e)
    Call(typ, fun, args) -> {
      let #(taken, i, fun) = unshadow(taken, i, fun)
      let #(taken, i, args) =
        list.fold(args, #(taken, i, []), fn(acc, arg) {
          let #(taken, i, l) = acc
          let #(taken, i, arg) = unshadow(taken, i, arg)
          #(taken, i, [arg, ..l])
        })
      let args = list.reverse(args)
      #(taken, i, Call(typ, fun, args))
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
              let #(taken, i, exp) = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
            False -> {
              let taken = [var, ..taken]
              let #(taken, i, exp) = unshadow(taken, i, exp)
              let vars = [var, ..vars]
              #(taken, i, vars, exp)
            }
          }
        })
      let #(taken, i, exp) = unshadow(taken, i, exp)
      let vars = list.reverse(vars)
      #(taken, i, Fn(typ, vars, exp))
    }
    Let(typ, var, val, exp) ->
      case list.contains(taken, var) {
        True -> {
          let #(taken, i, val) = unshadow(taken, i, val)
          let new_var = var <> "_V" <> int.to_string(i)
          let i = i + 1
          let exp = rename_var(var, new_var, exp)
          let var = new_var
          let taken = [var, ..taken]
          let #(taken, i, exp) = unshadow(taken, i, exp)
          #(taken, i, Let(typ, var, val, exp))
        }
        False -> {
          let #(taken, i, val) = unshadow(taken, i, val)
          let taken = [var, ..taken]
          let #(taken, i, exp) = unshadow(taken, i, exp)
          #(taken, i, Let(typ, var, val, exp))
        }
      }
    If(typ, cond, then_exp, else_exp) -> {
      let #(taken, i, cond) = unshadow(taken, i, cond)
      let #(taken, i, then_exp) = unshadow(taken, i, then_exp)
      let #(taken, i, else_exp) = unshadow(taken, i, else_exp)
      #(taken, i, If(typ, cond, then_exp, else_exp))
    }
  }
}

fn unshadow_context(c: Context) -> Context {
  // TODO do we need to unshadow based on globals?
  let functions =
    list.map(c.functions, fn(fun) {
      let #(taken, i, exp) = unshadow([], 1, fun.body)
      Function(fun.name, fun.typ, fun.params, exp)
    })
  Context(..c, functions: functions)
}

fn replace_var(replace: String, with: Exp, in: Exp) -> Exp {
  case in {
    Literal(_, _) -> in
    GlobalVar(_, _) -> in
    Var(_typ, var) ->
      case var == replace {
        True -> with
        False -> in
      }
    Call(typ, fun, args) -> {
      let fun = replace_var(replace, with, fun)
      let args = list.map(args, replace_var(replace, with, _))
      Call(typ, fun, args)
    }
    Fn(typ, vars, exp) ->
      case list.contains(vars, replace) {
        True -> in
        False -> Fn(typ, vars, replace_var(replace, with, exp))
      }
    Let(typ, var, val, exp) ->
      case var == replace {
        True -> Let(typ, var, replace_var(replace, with, val), exp)
        False ->
          Let(
            typ,
            var,
            replace_var(replace, with, val),
            replace_var(replace, with, exp),
          )
      }
    If(typ, cond, then_exp, else_exp) ->
      If(
        typ,
        replace_var(replace, with, cond),
        replace_var(replace, with, then_exp),
        replace_var(replace, with, else_exp),
      )
  }
}

fn rename_var(replace: String, with: String, in: Exp) -> Exp {
  case in {
    Literal(_, _) -> in
    GlobalVar(_, _) -> in
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
