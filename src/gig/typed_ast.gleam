import call_graph
import env
import glance as g
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/string
import graph

pub const builtin = "gleam"

pub const nil_type = NamedType("Nil", builtin, [])

pub const bool_type = NamedType("Bool", builtin, [])

pub const int_type = NamedType("Int", builtin, [])

pub const float_type = NamedType("Float", builtin, [])

pub const string_type = NamedType("String", builtin, [])

pub type Ref {
  Ref(id: Int)
}

pub type TypeVar {
  Bound(Type)
  Unbound(id: Int)
}

pub type Definition(definition) {
  Definition(attributes: List(Attribute), definition: definition)
}

pub type Attribute {
  Attribute(name: String, arguments: List(Expression))
}

pub type ValueKind {
  ConstructorFunction
  TopLevelFunction
}

// TODO it would probably be good to parameterise the Type
// then we can offer an ast with resolved types and also library users
// can add their own info
pub type Module {
  Module(
    name: String,
    // TODO move these into context instead of module?
    type_env: env.Env(String, #(Poly, List(Variant))),
    value_env: env.Env(String, #(Poly, List(Option(String)), ValueKind)),
    imports: List(Definition(Import)),
    custom_types: List(Definition(CustomType)),
    type_aliases: List(Definition(TypeAlias)),
    constants: List(Definition(Constant)),
    functions: List(Definition(Function)),
  )
}

pub type Function {
  Function(
    typ: Poly,
    name: String,
    publicity: Publicity,
    parameters: List(FunctionParameter),
    return: Option(Annotation),
    body: List(Statement),
    location: Span,
  )
}

pub type Span {
  /// A span within a file, indicated by byte offsets.
  Span(start: Int, end: Int)
}

pub type Statement {
  Use(typ: Type, patterns: List(Pattern), function: Expression)
  Assignment(
    typ: Type,
    kind: AssignmentKind,
    pattern: Pattern,
    annotation: Option(Annotation),
    value: Expression,
  )
  Expression(typ: Type, expression: Expression)
}

pub type AssignmentKind {
  Let
  Assert
}

pub type Pattern {
  PatternInt(typ: Type, value: String)
  PatternFloat(typ: Type, value: String)
  PatternString(typ: Type, value: String)
  PatternDiscard(typ: Type, name: String)
  PatternVariable(typ: Type, name: String)
  PatternTuple(typ: Type, elems: List(Pattern))
  PatternList(typ: Type, elements: List(Pattern), tail: Option(Pattern))
  PatternAssignment(typ: Type, pattern: Pattern, name: String)
  PatternConcatenate(typ: Type, left: String, right: AssignmentName)
  PatternBitString(
    typ: Type,
    segments: List(#(Pattern, List(BitStringSegmentOption(Pattern)))),
  )
  PatternConstructor(
    typ: Type,
    module: String,
    constructor: String,
    arguments: List(Field(Pattern)),
    ordered_arguments: List(Pattern),
    with_module: Bool,
    with_spread: Bool,
  )
}

pub type Expression {
  Int(typ: Type, value: String)
  Float(typ: Type, value: String)
  String(typ: Type, value: String)
  LocalVariable(typ: Type, name: String)
  // TODO GlobalVariable and ModuleAccess is very similar
  GlobalVariable(typ: Type, module: String, name: String)
  NegateInt(typ: Type, value: Expression)
  NegateBool(typ: Type, value: Expression)
  Block(typ: Type, statements: List(Statement))
  Panic(typ: Type, value: Option(Expression))
  Todo(typ: Type, value: Option(Expression))
  Tuple(typ: Type, elements: List(Expression))
  List(typ: Type, elements: List(Expression), rest: Option(Expression))
  Fn(
    typ: Type,
    parameters: List(FunctionParameter),
    return: Option(Annotation),
    body: List(Statement),
  )
  RecordUpdate(
    typ: Type,
    module: Option(String),
    constructor: String,
    record: Expression,
    fields: List(#(String, Expression)),
  )
  FieldAccess(
    typ: Type,
    container: Expression,
    module: String,
    variant: String,
    label: String,
    index: Int,
  )
  Call(
    typ: Type,
    function: Expression,
    arguments: List(Field(Expression)),
    ordered_arguments: List(Expression),
  )
  TupleIndex(typ: Type, tuple: Expression, index: Int)
  FnCapture(
    typ: Type,
    label: Option(String),
    function: Expression,
    arguments_before: List(Field(Expression)),
    arguments_after: List(Field(Expression)),
  )
  BitString(
    typ: Type,
    segments: List(#(Expression, List(BitStringSegmentOption(Expression)))),
  )
  Case(typ: Type, subjects: List(Expression), clauses: List(Clause))
  BinaryOperator(
    typ: Type,
    name: g.BinaryOperator,
    left: Expression,
    right: Expression,
  )
}

pub type Clause {
  Clause(
    patterns: List(List(Pattern)),
    guard: Option(Expression),
    body: Expression,
  )
}

pub type BitStringSegmentOption(t) {
  BinaryOption
  IntOption
  FloatOption
  BitStringOption
  Utf8Option
  Utf16Option
  Utf32Option
  Utf8CodepointOption
  Utf16CodepointOption
  Utf32CodepointOption
  SignedOption
  UnsignedOption
  BigOption
  LittleOption
  NativeOption
  SizeValueOption(t)
  SizeOption(Int)
  UnitOption(Int)
}

pub type FunctionParameter {
  FunctionParameter(
    typ: Type,
    label: Option(String),
    name: AssignmentName,
    annotation: Option(Annotation),
  )
}

pub type AssignmentName {
  Named(value: String)
  Discarded(value: String)
}

pub type Import {
  Import(
    module: String,
    alias: Option(AssignmentName),
    unqualified_types: List(UnqualifiedImport),
    unqualified_values: List(UnqualifiedImport),
  )
}

pub type Constant {
  Constant(
    name: String,
    publicity: Publicity,
    annotation: Option(Type),
    value: Expression,
  )
}

pub type UnqualifiedImport {
  UnqualifiedImport(name: String, alias: Option(String))
}

pub type Publicity {
  Public
  Private
}

pub type TypeAlias {
  TypeAlias(
    name: String,
    publicity: Publicity,
    parameters: List(String),
    aliased: Type,
  )
}

pub type CustomType {
  CustomType(
    typ: Poly,
    name: String,
    publicity: Publicity,
    opaque_: Bool,
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type Variant {
  Variant(typ: Poly, name: String, fields: List(Field(Annotation)))
}

pub type Field(t) {
  Field(label: Option(String), item: t)
}

pub type Type {
  // TODO change to module, name
  NamedType(name: String, module: String, parameters: List(Type))
  TupleType(elements: List(Type))
  FunctionType(parameters: List(Type), return: Type)
  VariableType(ref: Ref)
}

pub type Poly {
  // TODO should vars be List(TypeVarRef) ??
  Poly(vars: List(Int), typ: Type)
}

pub type Annotation {
  NamedAnno(
    typ: Type,
    name: String,
    module: Option(String),
    parameters: List(Annotation),
  )
  TupleAnno(typ: Type, elements: List(Annotation))
  FunctionAnno(typ: Type, parameters: List(Annotation), return: Annotation)
  VariableAnno(typ: Type, name: String)
  HoleAnno(typ: Type, name: String)
}

pub type Error {
  UnexpectedEndOfInput
  UnexpectedToken
}

pub type QualifiedName {
  QualifiedName(module: String, name: String)
}

pub type TypeVarEnv =
  env.Env(Ref, TypeVar)

pub type Context {
  Context(
    current_module: String,
    module_alias_env: env.Env(String, String),
    type_vars: TypeVarEnv,
    modules: env.Env(String, Module),
    type_uid: Int,
    temp_uid: Int,
  )
}

pub type LocalEnv =
  env.Env(String, Type)

pub type TypeEnv =
  env.Env(String, Type)

pub fn new_context() -> Context {
  Context(
    current_module: "",
    module_alias_env: env.new(),
    type_vars: env.new(),
    modules: env.new(),
    type_uid: 0,
    temp_uid: 0,
  )
}

pub fn infer_module(c: Context, module: g.Module, name: String) -> Context {
  io.debug("infer module " <> name)
  let c = Context(..c, current_module: name)

  // handle module imports
  let module_alias_env =
    list.fold(module.imports, env.new(), fn(n, imp) {
      let name = imp.definition.module
      let assert Ok(alias) = list.last(string.split(name, "/"))
      env.put(n, alias, name)
    })

  let c = Context(..c, module_alias_env:)

  // add types to env so they can reference eachother and themselves
  let c =
    list.fold(module.custom_types, c, fn(c, def) {
      // TODO instead of type var, we can figure out the type just from the
      // header (i.e. name + params), without inferring the constructors
      let #(c, typ) = new_type_var_ref(c)
      register_named_type(c, def.definition.name, Poly([], typ), [])
    })

  // now infer custom types fr fr
  let c =
    list.fold(module.custom_types, c, fn(c, def) {
      let #(c, custom) = infer_custom_type(c, def.definition)
      let c = register_named_type(c, custom.name, custom.typ, custom.variants)
      let attrs = infer_attributes(c, def.attributes)
      let def = Definition(attrs, custom)
      update_module(c, fn(mod) {
        Module(..mod, custom_types: [def, ..mod.custom_types])
      })
    })

  // create a function call graph to group mutually recursive functions
  let rec_groups =
    call_graph.create(module)
    |> graph.strongly_connected_components()

  // TODO handle external functions

  list.fold(rec_groups, c, fn(c, group) {
    // find the function definitions by name
    let assert Ok(group) =
      list.try_map(group, fn(fun_name) {
        list.find(module.functions, fn(f) { f.definition.name == fun_name })
      })

    // add functions to global env so they are available for recursion
    let c =
      list.fold(group, c, fn(c, fun) {
        let #(c, typ) = new_type_var_ref(c)
        let labels = list.map(fun.definition.parameters, fn(f) { f.label })
        register_value(
          c,
          fun.definition.name,
          Poly([], typ),
          labels,
          TopLevelFunction,
        )
      })

    // infer types for each function
    let #(c, group) =
      list.fold(group, #(c, []), fn(acc, def) {
        let #(c, group) = acc
        let #(c, fun) = infer_function(c, def.definition)
        let attrs = infer_attributes(c, def.attributes)
        let def = Definition(attrs, fun)
        #(c, [def, ..group])
      })

    // update context with fully inferred function
    list.fold(group, c, fn(c, def) {
      let fun = def.definition
      let labels = list.map(fun.parameters, fn(f) { f.label })
      let c = register_value(c, fun.name, fun.typ, labels, TopLevelFunction)
      update_module(c, fn(mod) {
        Module(..mod, functions: [def, ..mod.functions])
      })
    })
  })
}

fn generalise(c: Context, typ: Type) {
  let tvs =
    list.unique(find_tvs(c, typ))
    |> list.sort(int.compare)
  Poly(tvs, typ)
}

fn get_module_context(c: Context, module: String) -> Module {
  env.get(c.modules, module)
  |> result.unwrap(
    Module(
      name: module,
      type_env: env.new(),
      value_env: env.new(),
      imports: [],
      custom_types: [],
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
}

fn update_module(c: Context, fun: fn(Module) -> Module) {
  let module = get_module_context(c, c.current_module)
  let module = fun(module)
  let modules = env.put(c.modules, c.current_module, module)
  Context(..c, modules:)
}

fn register_value(
  c: Context,
  name: String,
  typ: Poly,
  labels: List(Option(String)),
  kind: ValueKind,
) -> Context {
  update_module(c, fn(module) {
    let value_env = env.put(module.value_env, name, #(typ, labels, kind))
    Module(..module, value_env:)
  })
}

fn register_named_type(
  c: Context,
  name: String,
  typ: Poly,
  variants: List(Variant),
) -> Context {
  update_module(c, fn(module) {
    let type_env = env.put(module.type_env, name, #(typ, variants))
    Module(..module, type_env:)
  })
}

fn infer_attributes(c: Context, attrs: List(g.Attribute)) {
  let attr_env = env.new()
  let attr_env = env.put(attr_env, "c", nil_type)

  let #(c, attrs) =
    list.fold(attrs, #(c, []), fn(acc, attr) {
      let #(c, attrs) = acc
      let #(c, exprs) =
        list.fold(attr.arguments, #(c, []), fn(acc, attr) {
          let #(c, exprs) = acc
          let assert Ok(#(c, expr)) = infer_expression(c, attr_env, attr)
          #(c, [expr, ..exprs])
        })
      let exprs = list.reverse(exprs)
      let attr = Attribute(attr.name, exprs)
      #(c, [attr, ..attrs])
    })
  list.reverse(attrs)
}

fn infer_function(c: Context, fun: g.Function) {
  let #(c, parameters, return) =
    infer_function_parameters(c, fun.parameters, fun.return)

  let #(c, return_type) = case return {
    Some(x) -> #(c, x.typ)
    None -> new_type_var_ref(c)
  }

  // put params into local env
  let n =
    list.fold(parameters, env.new(), fn(n, param) {
      case param.name {
        Named(name) -> env.put(n, name, param.typ)
        Discarded(_) -> n
      }
    })

  // infer body
  let #(c, body) = infer_body(c, n, fun.body)

  // compute function type
  let parameter_types = list.map(parameters, fn(x) { x.typ })
  let typ = FunctionType(parameter_types, return_type)

  // unify the return type with the last statement
  let c = case list.last(body) {
    Ok(statement) -> unify(c, return_type, statement.typ)
    Error(_) -> c
  }

  let name = fun.name
  let publicity = case fun.publicity {
    g.Public -> Public
    g.Private -> Private
  }
  let location = Span(fun.location.start, fun.location.end)

  let typ = generalise(c, typ)

  let fun =
    Function(typ:, name:, publicity:, parameters:, return:, body:, location:)
  #(c, fun)
}

fn infer_custom_type(c: Context, custom: g.CustomType) {
  // create a type variable for each parameter
  // these will be used when a field references a type parameter
  let #(c, parameters) =
    list.fold(custom.parameters, #(c, []), fn(acc, p) {
      let #(c, l) = acc
      let #(c, typ) = new_type_var_ref(c)
      #(c, [#(p, typ), ..l])
    })
  let parameters = list.reverse(parameters)

  let param_types = list.map(parameters, fn(x) { x.1 })
  let module = c.current_module
  let name = custom.name
  let typ = NamedType(module:, name:, parameters: param_types)

  // create an env for param types
  let n = list.fold(parameters, env.new(), fn(n, p) { env.put(n, p.0, p.1) })

  // process each variant
  let #(c, variants) =
    list.fold(custom.variants, #(c, []), fn(acc, variant) {
      let #(c, l) = acc
      let #(c, v) = infer_variant(c, n, typ, variant)
      #(c, [v, ..l])
    })
  let variants = list.reverse(variants)

  let opaque_ = custom.opaque_
  let publicity = case custom.publicity {
    g.Public -> Public
    g.Private -> Private
  }
  let parameters = custom.parameters

  let typ = generalise(c, typ)

  let custom =
    CustomType(typ:, opaque_:, name:, publicity:, parameters:, variants:)

  #(c, custom)
}

fn infer_variant(c, n, typ: Type, variant: g.Variant) -> #(Context, Variant) {
  let #(c, fields) =
    list.fold(variant.fields, #(c, []), fn(acc, field) {
      let #(c, fields) = acc
      let #(c, annotation) = do_infer_annotation(c, n, field.item)
      let label = field.label
      let field = Field(label, annotation)
      #(c, [field, ..fields])
    })
  let fields = list.reverse(fields)

  let types = list.map(fields, fn(f) { f.item.typ })
  let labels = list.map(fields, fn(f) { f.label })

  // handle 0 parameter variants are not functions
  let #(c, typ) = case types {
    [] -> #(c, typ)
    _ -> #(c, FunctionType(types, typ))
  }
  let typ = generalise(c, typ)

  let c = register_value(c, variant.name, typ, labels, ConstructorFunction)

  #(c, Variant(typ, variant.name, fields))
}

fn find_vars_in_type(t: g.Type) -> List(String) {
  case t {
    g.NamedType(_name, _module, parameters) ->
      list.flat_map(parameters, find_vars_in_type)
    g.TupleType(elements) -> list.flat_map(elements, find_vars_in_type)
    g.FunctionType(parameters, return) ->
      list.flat_map([return, ..parameters], find_vars_in_type)
    g.VariableType(name) -> [name]
    g.HoleType(_) -> []
  }
}

fn infer_function_parameters(
  c: Context,
  parameters: List(g.FunctionParameter),
  return: Option(g.Type),
) -> #(Context, List(FunctionParameter), Option(Annotation)) {
  // find type variables used in the function's parameters
  let vars =
    list.flat_map(parameters, fn(param) {
      case param.type_ {
        Some(typ) -> find_vars_in_type(typ)
        None -> []
      }
    })

  // also look for varaibles in the return type
  let vars = case return {
    Some(ret) -> list.append(find_vars_in_type(ret), vars)
    None -> vars
  }

  // sort by name just for fun
  let vars =
    vars
    |> list.unique()
    |> list.sort(string.compare)

  // create an env for the type variables
  let #(c, type_env) =
    list.fold(vars, #(c, env.new()), fn(acc, name) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = env.put(n, name, typ)
      #(c, n)
    })

  // create type vars for parameters
  let #(c, params) =
    list.fold(parameters, #(c, []), fn(acc, param) {
      let #(c, param_types) = acc

      let label = param.label

      let name = case param.name {
        g.Named(s) -> Named(s)
        g.Discarded(s) -> Discarded(s)
      }

      let #(c, annotation) = case param.type_ {
        Some(typ) -> {
          let #(c, anno) = do_infer_annotation(c, type_env, typ)
          #(c, Some(anno))
        }
        None -> #(c, None)
      }

      let #(c, typ) = case annotation {
        Some(a) -> #(c, a.typ)
        None -> new_type_var_ref(c)
      }

      #(c, [FunctionParameter(typ, label, name, annotation), ..param_types])
    })
  let params = list.reverse(params)

  // handle function return type
  let #(c, return) = case return {
    Some(typ) -> {
      let #(c, anno) = do_infer_annotation(c, type_env, typ)
      #(c, Some(anno))
    }
    None -> #(c, None)
  }

  #(c, params, return)
}

fn do_infer_annotation(
  c: Context,
  n: TypeEnv,
  typ: g.Type,
) -> #(Context, Annotation) {
  case typ {
    g.NamedType(name, anno_module, params) -> {
      let #(c, params) =
        list.fold(params, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = do_infer_annotation(c, n, p)
          #(c, [p, ..l])
        })
      let params = list.reverse(params)

      let assert Ok(#(QualifiedName(module, name), _poly, _variants)) =
        resolve_type_name(c, anno_module, name)
      // TODO check correct type params were used (match with poly?)

      let typ = NamedType(name, module, list.map(params, fn(x) { x.typ }))
      #(c, NamedAnno(typ, name, anno_module, params))
    }
    g.TupleType(elements) -> {
      let #(c, elements) =
        list.fold(elements, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = do_infer_annotation(c, n, p)
          #(c, [p, ..l])
        })
      let elements = list.reverse(elements)
      let typ = TupleType(list.map(elements, fn(x) { x.typ }))
      #(c, TupleAnno(typ, elements))
    }
    g.FunctionType(parameters, return) -> {
      let #(c, params) =
        list.fold(parameters, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, p) = do_infer_annotation(c, n, p)
          #(c, [p, ..l])
        })
      let params = list.reverse(params)
      let #(c, ret) = do_infer_annotation(c, n, return)
      let typ = FunctionType(list.map(params, fn(x) { x.typ }), ret.typ)
      #(c, FunctionAnno(typ, params, ret))
    }
    g.VariableType(name) -> {
      let assert Ok(typ) = env.get(n, name)
      #(c, VariableAnno(typ, name))
    }
    g.HoleType(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      #(c, HoleAnno(typ, name))
    }
  }
}

type ResolvedName {
  LocalName(name: String, typ: Type)
  GlobalName(name: QualifiedName, typ: Poly, labels: List(Option(String)))
}

fn resolve_unqualified_name(
  c: Context,
  n: LocalEnv,
  name: String,
) -> Result(ResolvedName, String) {
  case env.get(n, name) {
    // try local env
    Ok(typ) -> Ok(LocalName(name, typ))
    Error(_) ->
      case resolve_unqualified_global(c, name) {
        Ok(#(name, typ, labels)) -> Ok(GlobalName(name, typ, labels))
        Error(e) -> Error(e)
      }
  }
}

fn resolve_unqualified_global(
  c: Context,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Option(String))), String) {
  // try global env
  case resolve_global_name(c, c.current_module, name) {
    Ok(#(name, typ, labels, _)) -> Ok(#(name, typ, labels))
    // try prelude
    Error(_) -> {
      case resolve_global_name(c, builtin, name) {
        Ok(#(name, typ, labels, _)) -> Ok(#(name, typ, labels))
        Error(_) -> Error("could not resolve name " <> name)
      }
    }
  }
}

fn resolve_aliased_name(
  c: Context,
  name: QualifiedName,
) -> Result(#(QualifiedName, Poly, List(Option(String)), ValueKind), String) {
  case env.get(c.module_alias_env, name.module) {
    Ok(module) -> resolve_global_name(c, module, name.name)
    Error(_) -> Error("Could not resolve module name " <> name.module)
  }
}

pub fn resolve_global_name(
  c: Context,
  module_name: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Option(String)), ValueKind), String) {
  case env.get(c.modules, module_name) {
    Ok(module) ->
      case env.get(module.value_env, name) {
        Ok(#(typ, labels, kind)) ->
          Ok(#(QualifiedName(module_name, name), typ, labels, kind))
        Error(_) -> Error("Could not resolve name " <> name)
      }
    Error(_) -> Error("Missing module " <> module_name)
  }
}

fn resolve_type_name(
  c: Context,
  mod: Option(String),
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), String) {
  case mod {
    Some(mod) -> resolve_aliased_type_name(c, mod, name)
    None ->
      case resolve_global_type_name(c, c.current_module, name) {
        Ok(result) -> Ok(result)
        Error(_) ->
          case resolve_global_type_name(c, builtin, name) {
            Ok(result) -> Ok(result)
            Error(_) -> Error("Could not resolve type " <> name)
          }
      }
  }
}

fn resolve_aliased_type_name(
  c: Context,
  module: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), String) {
  case env.get(c.module_alias_env, module) {
    Ok(module) -> resolve_global_type_name(c, module, name)
    Error(_) -> Error("Could not resolve module name " <> module)
  }
}

pub fn resolve_global_type_name(
  c: Context,
  module: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), String) {
  case env.get(c.modules, module) {
    Ok(c) ->
      case env.get(c.type_env, name) {
        Ok(#(typ, variants)) ->
          Ok(#(QualifiedName(module, name), typ, variants))
        Error(_) -> Error("Could not resolve type " <> name)
      }
    Error(_) -> Error("Missing module " <> module)
  }
}

fn resolve_constructor_name(c: Context, mod: Option(String), name: String) {
  case mod {
    Some(mod) -> {
      let q_name = QualifiedName(mod, name)
      resolve_aliased_name(c, q_name)
      |> result.map(fn(global) { #(global.0, global.1, global.2) })
    }
    None -> {
      resolve_unqualified_global(c, name)
    }
  }
}

fn new_type_var_ref(c: Context) {
  let ref = Ref(c.type_uid)
  let type_vars = env.put(c.type_vars, ref, Unbound(c.type_uid))
  let typ = VariableType(ref)
  #(Context(..c, type_vars: type_vars, type_uid: c.type_uid + 1), typ)
}

fn infer_pattern(
  c: Context,
  n: LocalEnv,
  pattern: g.Pattern,
) -> #(Context, LocalEnv, Pattern) {
  case pattern {
    g.PatternInt(value) -> #(c, n, PatternInt(int_type, value))
    g.PatternFloat(value) -> #(c, n, PatternInt(float_type, value))
    g.PatternString(value) -> #(c, n, PatternInt(string_type, value))
    g.PatternDiscard(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      #(c, n, PatternDiscard(typ, name))
    }
    g.PatternVariable(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      let pattern = PatternVariable(typ, name)
      let n = env.put(n, name, typ)
      #(c, n, pattern)
    }
    g.PatternTuple(elems) -> {
      // Infer types for all elements in the tuple pattern
      let #(c, n, elems) =
        list.fold(elems, #(c, n, []), fn(acc, elem) {
          let #(c, n, patterns) = acc
          let #(c, n, pattern) = infer_pattern(c, n, elem)
          #(c, n, [pattern, ..patterns])
        })
      let elems = list.reverse(elems)

      // Create the tuple type from the inferred element types
      let typ = TupleType(list.map(elems, fn(e) { e.typ }))

      #(c, n, PatternTuple(typ, elems))
    }
    g.PatternList(elements, tail) -> {
      // Infer types for all elements in the list pattern
      let #(c, n, elements) =
        list.fold(elements, #(c, n, []), fn(acc, elem) {
          let #(c, n, patterns) = acc
          let #(c, n, pattern) = infer_pattern(c, n, elem)
          #(c, n, [pattern, ..patterns])
        })
      let elements = list.reverse(elements)

      // Create a type variable for the element type
      let #(c, elem_type) = new_type_var_ref(c)

      // Unify all element types with the element type variable
      let c =
        list.fold(elements, c, fn(c, elem) { unify(c, elem.typ, elem_type) })

      // Create the list type
      let typ = NamedType("List", builtin, [elem_type])

      // Handle the tail pattern if present
      let #(c, n, tail) = case tail {
        Some(tail_pattern) -> {
          let #(c, n, tail) = infer_pattern(c, n, tail_pattern)
          // The tail should be a list of the same type
          let c = unify(c, tail.typ, typ)
          #(c, n, Some(tail))
        }
        None -> #(c, n, None)
      }

      #(c, n, PatternList(typ, elements, tail))
    }
    g.PatternAssignment(pattern, name) -> {
      // First, infer the type of the inner pattern
      let #(c, n, pattern) = infer_pattern(c, n, pattern)

      // Create the PatternAssignment with the same type as the inner pattern
      let pattern = PatternAssignment(pattern.typ, pattern, name)

      // Add the name binding to the environment
      let n = env.put(n, name, pattern.typ)

      #(c, n, pattern)
    }
    g.PatternConcatenate(left, right) -> {
      // If it's not discarded, add right as a string variable
      let #(n, right) = case right {
        g.Named(name) -> {
          let n = env.put(n, name, string_type)
          #(n, Named(name))
        }
        g.Discarded(name) -> #(n, Discarded(name))
      }

      // Create the concatenation pattern
      let pattern = PatternConcatenate(string_type, left, right)

      #(c, n, pattern)
    }
    g.PatternBitString(segs) -> todo
    g.PatternConstructor(module, constructor, arguments, with_spread) -> {
      // resolve the constructor function
      let assert Ok(#(name, poly, labels)) =
        resolve_constructor_name(c, module, constructor)

      let with_module = case module {
        Some(_) -> True
        None -> False
      }

      let module = name.module
      let constructor = name.name

      // infer the type of all arguments
      let #(c, n, arguments) =
        list.fold(arguments, #(c, n, []), fn(acc, arg) {
          let #(c, n, arguments) = acc
          let #(c, n, arg2) = infer_pattern(c, n, arg.item)
          #(c, n, [Field(arg.label, arg2), ..arguments])
        })

      // handle labels
      let ordered_arguments = resolve_labels_ordered(arguments, labels)
      let arg_types = list.map(ordered_arguments, fn(x) { x.typ })

      // handle 0 parameter variants are not functions
      let #(c, typ) = case arg_types {
        [] -> instantiate(c, poly)
        _ -> {
          // unify the constructor function type with the types of args
          let #(c, fun_typ) = instantiate(c, poly)
          let #(c, typ) = new_type_var_ref(c)
          let c = unify(c, fun_typ, FunctionType(arg_types, typ))
          #(c, typ)
        }
      }

      let pattern =
        PatternConstructor(
          typ:,
          module:,
          constructor:,
          arguments:,
          ordered_arguments:,
          with_module:,
          with_spread:,
        )

      #(c, n, pattern)
    }
  }
}

fn infer_annotation(c: Context, typ: g.Type) -> #(Context, Annotation) {
  let vars =
    find_vars_in_type(typ)
    |> list.unique()
    |> list.sort(string.compare)

  let #(c, type_env) =
    list.fold(vars, #(c, env.new()), fn(acc, name) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = env.put(n, name, typ)
      #(c, n)
    })

  do_infer_annotation(c, type_env, typ)
}

fn infer_body(
  c: Context,
  n: LocalEnv,
  body: List(g.Statement),
) -> #(Context, List(Statement)) {
  case body {
    [] -> #(c, [])
    [x, ..xs] ->
      case x {
        g.Expression(value) -> {
          let assert Ok(#(c, value)) = infer_expression(c, n, value)

          let statement = Expression(value.typ, value)

          // infer the rest of the body
          let #(c, rest) = infer_body(c, n, xs)
          #(c, [statement, ..rest])
        }
        g.Assignment(
          value: value,
          pattern: pattern,
          annotation: annotation,
          kind: kind,
        ) -> {
          // infer pattern, annotation, and value
          let #(c, n, pattern) = infer_pattern(c, n, pattern)

          // if there is an annotation, the pattern must unify with the annotation
          let #(c, annotation) = case annotation {
            Some(typ) -> {
              let #(c, annotation) = infer_annotation(c, typ)
              let c = unify(c, pattern.typ, annotation.typ)
              #(c, Some(annotation))
            }
            None -> #(c, None)
          }

          let assert Ok(#(c, value)) = infer_expression(c, n, value)

          // the pattern must unify with both the annotation
          // and the assigned value
          let c = unify(c, pattern.typ, value.typ)

          // TODO check the right "kind" was used (needs exhaustive checking)
          let kind = case kind {
            g.Let -> Let
            g.Assert -> Assert
          }

          let statement =
            Assignment(
              typ: value.typ,
              value: value,
              pattern: pattern,
              annotation: annotation,
              kind: kind,
            )

          // infer the rest of the body
          let #(c, rest) = infer_body(c, n, xs)
          #(c, [statement, ..rest])
        }
        g.Use(pats, fun) -> {
          // infer each of the patterns
          let #(c, n, pats) =
            list.fold(pats, #(c, n, []), fn(acc, pat) {
              let #(c, n, patterns) = acc
              let #(c, n, pattern) = infer_pattern(c, n, pat)
              #(c, n, [pattern, ..patterns])
            })
          let pats = list.reverse(pats)
          let pat_types = list.map(pats, fn(p) { p.typ })

          // TODO we can't just infer this because its missing the last parameter
          // is fun always a Call node?
          let assert Ok(#(c, fun)) = infer_expression(c, n, fun)

          // 1. fun must be a function type
          // 2. fun must have callback as last parameter
          // 3. callback arg types must match pattern types

          todo
        }
      }
  }
}

fn resolve_labels_ordered(args: List(Field(a)), params: List(Option(String))) {
  // unify by position until reaching a labeled arg, at which point switch to unify_labels
  case args, params {
    [], [] -> []
    [], _ -> panic as "not enough arguments"
    _, [] -> panic as "too many arguments"
    [a, ..a_rest], [_p, ..p_rest] ->
      case a.label {
        Some(_) -> resolve_labels(args, params)
        None -> [a.item, ..resolve_labels_ordered(a_rest, p_rest)]
      }
  }
}

fn resolve_labels(args: List(Field(a)), params: List(Option(String))) {
  // find the labels in the order specified by parameters
  case params {
    [] -> []
    [p, ..p_rest] ->
      case list.pop(args, fn(a) { a.label == p }) {
        Ok(#(a, a_rest)) -> [a.item, ..resolve_labels(a_rest, p_rest)]
        Error(_) -> panic as { "no matching label" }
      }
  }
}

fn infer_expression(
  c: Context,
  n: LocalEnv,
  exp: g.Expression,
) -> Result(#(Context, Expression), String) {
  case exp {
    g.Int(s) -> Ok(#(c, Int(int_type, s)))
    g.Float(s) -> Ok(#(c, Float(float_type, s)))
    g.String(s) -> Ok(#(c, String(string_type, s)))
    g.Variable(s) -> {
      let name = resolve_unqualified_name(c, n, s)
      case name {
        Ok(GlobalName(name, typ, labels)) -> {
          let #(c, typ) = instantiate(c, typ)
          Ok(#(c, GlobalVariable(typ, name.module, name.name)))
        }
        Ok(LocalName(name, typ)) -> {
          Ok(#(c, LocalVariable(typ, name)))
        }
        Error(s) -> Error(s)
      }
    }
    g.NegateInt(e) -> {
      use #(c, e) <- try(infer_expression(c, n, e))
      let c = unify(c, e.typ, int_type)
      Ok(#(c, NegateInt(int_type, e)))
    }
    g.NegateBool(e) -> {
      use #(c, e) <- try(infer_expression(c, n, e))
      let c = unify(c, e.typ, bool_type)
      Ok(#(c, NegateBool(bool_type, e)))
    }
    g.Block(statements) -> {
      let #(c, statements) = infer_body(c, n, statements)

      case list.last(statements) {
        Ok(last) -> Ok(#(c, Block(last.typ, statements)))
        Error(_) -> Error("empty block")
      }
    }
    g.Panic(e) -> {
      case e {
        Some(e) -> {
          // the expression should be a string
          use #(c, e) <- try(infer_expression(c, n, e))
          unify(c, e.typ, string_type)
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Panic(typ, Some(e))))
        }
        None -> {
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Panic(typ, None)))
        }
      }
    }
    g.Todo(e) -> {
      case e {
        Some(e) -> {
          // the expression should be a string
          use #(c, e) <- try(infer_expression(c, n, e))
          unify(c, e.typ, string_type)
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Todo(typ, Some(e))))
        }
        None -> {
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Todo(typ, None)))
        }
      }
    }
    g.Tuple(elements) -> {
      // Infer type of all elements
      use #(c, elements) <- try(
        list.try_fold(elements, #(c, []), fn(acc, e) {
          let #(c, elements) = acc
          use #(c, e) <- try(infer_expression(c, n, e))
          Ok(#(c, [e, ..elements]))
        }),
      )
      let elements = list.reverse(elements)

      // Create tuple type
      let types = list.map(elements, fn(e) { e.typ })
      let typ = TupleType(types)
      Ok(#(c, Tuple(typ, elements)))
    }
    g.List(elements, tail) -> {
      // Infer types for all elements
      use #(c, elements) <- try(
        list.try_fold(elements, #(c, []), fn(acc, e) {
          let #(c, elements) = acc
          use #(c, e) <- try(infer_expression(c, n, e))
          Ok(#(c, [e, ..elements]))
        }),
      )
      let elements = list.reverse(elements)

      // Infer type for tail (if present)
      use #(c, tail) <- try(case tail {
        Some(t) -> {
          use #(c, t) <- try(infer_expression(c, n, t))
          Ok(#(c, Some(t)))
        }
        None -> Ok(#(c, None))
      })

      // Create a type variable for the element type
      let #(c, elem_type) = new_type_var_ref(c)
      let typ = NamedType("List", builtin, [elem_type])

      // Unify all element types
      let c = list.fold(elements, c, fn(c, e) { unify(c, e.typ, elem_type) })

      // Unify tail type with list type (if tail is present)
      let c = case tail {
        Some(t) -> unify(c, t.typ, typ)
        None -> c
      }

      Ok(#(c, List(typ, elements, tail)))
    }
    g.Fn(parameters, return, body) -> {
      // map parameters to FunctionParameter for code reuse
      let parameters =
        list.map(parameters, fn(p) {
          g.FunctionParameter(None, p.name, p.type_)
        })

      let #(c, parameters, return) =
        infer_function_parameters(c, parameters, return)

      let #(c, return_type) = case return {
        Some(x) -> #(c, x.typ)
        None -> new_type_var_ref(c)
      }

      // put params into local env
      let n =
        list.fold(parameters, env.new(), fn(n, param) {
          case param.name {
            Named(name) -> env.put(n, name, param.typ)
            Discarded(_) -> n
          }
        })

      // infer body
      let #(c, body) = infer_body(c, n, body)

      // compute function type
      let parameter_types = list.map(parameters, fn(x) { x.typ })
      let typ = FunctionType(parameter_types, return_type)

      // unify the return type with the last statement
      let c = case list.last(body) {
        Ok(statement) -> unify(c, return_type, statement.typ)
        Error(_) -> c
      }

      let fun = Fn(typ:, parameters:, return:, body:)
      Ok(#(c, fun))
    }
    g.RecordUpdate(module, constructor, expression, fields) -> {
      // Infer the type of the base record expression
      use #(c, base_expr) <- try(infer_expression(c, n, expression))

      // Resolve the constructor type
      let assert Ok(#(qualified_name, poly, labels)) =
        resolve_constructor_name(c, module, constructor)

      // Instantiate the constructor type
      let #(c, constructor_type) = instantiate(c, poly)

      // Unify the base expression type with the constructor type
      let c = unify(c, base_expr.typ, constructor_type)

      // Infer types for all updated fields
      use #(c, updated_fields) <- try(
        list.try_fold(fields, #(c, []), fn(acc, field) {
          let #(c, updated_fields) = acc
          use #(c, value) <- try(infer_expression(c, n, field.1))
          Ok(#(c, [#(field.0, value), ..updated_fields]))
        }),
      )
      let updated_fields = list.reverse(updated_fields)

      // The result type is the same as the constructor type
      let typ = constructor_type

      // Create the RecordUpdate expression
      let record_update =
        RecordUpdate(
          typ: typ,
          module: module,
          constructor: constructor,
          record: base_expr,
          fields: updated_fields,
        )

      Ok(#(c, record_update))
    }
    g.FieldAccess(container, label) -> {
      // TODO module access:
      // if it succeeds as the regular field access
      //   return as field access
      // else
      //   try to resolve module acccess
      let field_access = case infer_expression(c, n, container) {
        Ok(#(c, container)) -> {
          case resolve_type(c, container.typ) {
            NamedType(name, module, params) -> {
              let assert Ok(#(_, _, variants)) =
                resolve_aliased_type_name(c, module, name)
              case variants {
                [v] ->
                  v.fields
                  |> list.index_map(fn(x, i) { #(x, i) })
                  |> list.find_map(fn(field) {
                    case { field.0 }.label == Some(label) {
                      True ->
                        Ok(#(c, container, module, v.name, field.0, field.1))
                      False -> Error(Nil)
                    }
                  })
                _ -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
      case field_access {
        Ok(#(c, container, module, variant, field, index)) -> {
          // TODO need to instantiate this type somehow
          // using the container's type to fill in variables
          // (the field comes straight from the type definition)
          let typ = field.item.typ
          let field_access =
            FieldAccess(typ, container, module, variant, label, index)
          Ok(#(c, field_access))
        }
        Error(_) -> {
          case container {
            g.Variable(module) -> {
              case resolve_aliased_name(c, QualifiedName(module, label)) {
                Ok(#(name, poly, _labels, _)) -> {
                  let #(c, typ) = instantiate(c, poly)
                  Ok(#(c, GlobalVariable(typ, name.module, name.name)))
                }
                Error(s) -> Error(s)
              }
            }
            _ -> Error("invalid field access")
          }
        }
      }
    }
    g.Call(fun, args) -> {
      // infer the type of the function
      use #(c, fun) <- try(infer_expression(c, n, fun))

      // infer the type of all args
      use #(c, args) <- try(
        list.try_fold(args, #(c, []), fn(acc, field) {
          let #(c, args) = acc
          use #(c, arg) <- try(infer_expression(c, n, field.item))
          Ok(#(c, [Field(field.label, arg), ..args]))
        }),
      )
      let args = list.reverse(args)

      // handle labels
      let labels = case fun {
        GlobalVariable(_typ, module, name) -> {
          let assert Ok(#(_name, _poly, labels, _)) =
            resolve_global_name(c, module, name)
          labels
        }
        _ -> list.map(args, fn(_) { None })
      }
      let ordered_args = resolve_labels_ordered(args, labels)
      let arg_types = list.map(ordered_args, fn(x) { x.typ })

      // unify the function type with the types of args
      let #(c, typ) = new_type_var_ref(c)
      let c = unify(c, fun.typ, FunctionType(arg_types, typ))

      Ok(#(c, Call(typ, fun, args, ordered_args)))
    }
    g.TupleIndex(tuple, index) -> {
      use #(c, tuple) <- try(infer_expression(c, n, tuple))
      case tuple.typ {
        TupleType(elements) -> {
          let typ = tuple_index_type(c, elements, index)
          Ok(#(c, TupleIndex(typ, tuple, index)))
        }
        _ -> Error("tuple index on non-tuple")
      }
    }
    g.FnCapture(label, fun, before, after) -> {
      // 1. infer function
      // 2. infer args
      // 3. new type var for blank
      // 4. unify function with args and blank
      todo as "FnCapture"
    }
    g.BitString(segs) -> {
      todo as "BitString"
    }
    g.Case(subjects, clauses) -> {
      use #(c, subjects) <- try(
        list.try_fold(subjects, #(c, []), fn(acc, sub) {
          let #(c, subjects) = acc
          use #(c, sub) <- try(infer_expression(c, n, sub))
          Ok(#(c, [sub, ..subjects]))
        }),
      )
      let subjects = list.reverse(subjects)

      // all of the branches should unify with the case type
      let #(c, typ) = new_type_var_ref(c)

      use #(c, clauses) <- try(
        list.try_fold(clauses, #(c, []), fn(acc, clause) {
          let #(c, clauses) = acc

          // patterns is a List(List(Pattern))
          // the inner list has a pattern to match each subject
          // the outer list has alternatives that have the same body
          let #(c, n, patterns) =
            list.fold(clause.patterns, #(c, n, []), fn(acc, pat) {
              let #(c, n, pats) = acc

              // each pattern has a corresponding subject
              let assert Ok(sub_pats) = list.strict_zip(subjects, pat)

              let #(c, n, pat) =
                list.fold(sub_pats, #(c, n, []), fn(acc, sub_pat) {
                  let #(c, n, pats) = acc
                  let #(sub, pat) = sub_pat
                  let #(c, n, pat) = infer_pattern(c, n, pat)
                  // the pattern type should match the corresponding subject
                  let c = unify(c, pat.typ, sub.typ)
                  #(c, n, [pat, ..pats])
                })

              // all alternatives must bind the same names
              // TODO check the alternative patterns bind the same names
              // how do we check this? do we need to unify (based on name)?
              // maybe infer_pattern needs to return a list of bindings
              // instead of a new env

              #(c, n, [pat, ..pats])
            })
          let patterns = list.reverse(patterns)

          // if the guard exists ensure it has a boolean result
          use #(c, guard) <- try(case clause.guard {
            Some(guard) -> {
              use #(c, guard) <- try(infer_expression(c, n, guard))
              let c = unify(c, guard.typ, bool_type)
              Ok(#(c, Some(guard)))
            }
            None -> Ok(#(c, None))
          })

          use #(c, body) <- try(infer_expression(c, n, clause.body))

          // the body should unify with the case type
          let c = unify(c, typ, body.typ)

          let santa = Clause(patterns:, guard:, body:)
          Ok(#(c, [santa, ..clauses]))
        }),
      )
      let clauses = list.reverse(clauses)

      Ok(#(c, Case(typ:, subjects:, clauses:)))
    }
    g.BinaryOperator(name, left, right) -> {
      let #(c, fun_typ) = case name {
        // Boolean logic
        g.And | g.Or -> #(c, FunctionType([bool_type, bool_type], bool_type))

        // Equality
        g.Eq | g.NotEq -> {
          let #(c, a) = new_type_var_ref(c)
          #(c, FunctionType([a, a], bool_type))
        }

        // Order comparison
        g.LtInt | g.LtEqInt | g.GtEqInt | g.GtInt -> #(
          c,
          FunctionType([int_type, int_type], bool_type),
        )

        g.LtFloat | g.LtEqFloat | g.GtEqFloat | g.GtFloat -> #(
          c,
          FunctionType([float_type, float_type], bool_type),
        )

        // Functions
        // TODO handle pipe in a seperate BinaryOperator case
        g.Pipe -> todo as "Pipe"

        // Maths
        g.AddInt | g.SubInt | g.MultInt | g.DivInt | g.RemainderInt -> #(
          c,
          FunctionType([int_type, int_type], int_type),
        )

        g.AddFloat | g.SubFloat | g.MultFloat | g.DivFloat -> #(
          c,
          FunctionType([float_type, float_type], float_type),
        )

        // Strings
        g.Concatenate -> #(
          c,
          FunctionType([string_type, string_type], string_type),
        )
      }

      use #(c, left) <- try(infer_expression(c, n, left))
      use #(c, right) <- try(infer_expression(c, n, right))

      // unify the function type with the types of args
      let #(c, typ) = new_type_var_ref(c)
      let c = unify(c, fun_typ, FunctionType([left.typ, right.typ], typ))

      Ok(#(c, BinaryOperator(typ, name, left, right)))
    }
  }
}

fn tuple_index_type(c: Context, elements: List(Type), index: Int) {
  // if the index is 0, return the first element
  // otherwise skip the first element and decrement the index
  case index, elements {
    0, [x, ..] -> x
    _, [_, ..xs] -> tuple_index_type(c, xs, index - 1)
    _, _ -> panic as "tuple index out of bounds"
  }
}

type PolyEnv =
  env.Env(Int, Type)

fn get_type_var(c: Context, var: Ref) {
  let assert Ok(x) = env.get(c.type_vars, var)
  x
}

fn set_type_var(c: Context, var: Ref, bind: TypeVar) {
  Context(..c, type_vars: env.put(c.type_vars, var, bind))
}

fn instantiate(c: Context, poly: Poly) -> #(Context, Type) {
  let #(c, n) =
    list.fold(poly.vars, #(c, env.new()), fn(acc, var) {
      let #(c, n) = acc
      let #(c, new_var) = new_type_var_ref(c)
      let n = env.put(n, var, new_var)
      #(c, n)
    })
  let typ = do_instantiate(c, n, poly.typ)
  #(c, typ)
}

fn find_tvs(c: Context, t: Type) -> List(Int) {
  case t {
    VariableType(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> find_tvs(c, x)
        Unbound(x) -> [x]
      }
    NamedType(_, _, args) -> list.flat_map(args, find_tvs(c, _))
    FunctionType(args, ret) -> list.flat_map([ret, ..args], find_tvs(c, _))
    TupleType(elements) -> list.flat_map(elements, find_tvs(c, _))
  }
}

fn do_instantiate(c: Context, n: PolyEnv, typ: Type) -> Type {
  case typ {
    VariableType(ref) ->
      case get_type_var(c, ref) {
        Bound(x) -> do_instantiate(c, n, x)
        Unbound(x) ->
          case env.get(n, x) {
            Ok(r) -> r
            Error(_) -> typ
          }
      }
    NamedType(name, module, args) ->
      NamedType(name, module, list.map(args, do_instantiate(c, n, _)))
    FunctionType(args, ret) ->
      FunctionType(
        list.map(args, do_instantiate(c, n, _)),
        do_instantiate(c, n, ret),
      )
    TupleType(elements) ->
      TupleType(list.map(elements, do_instantiate(c, n, _)))
  }
}

fn unify(c: Context, a: Type, b: Type) -> Context {
  let a = resolve_type(c, a)
  let b = resolve_type(c, b)
  case a, b {
    VariableType(ref), b ->
      case a == b {
        True -> c
        False -> {
          let assert Unbound(aid) = get_type_var(c, ref)
          let #(c, occurs) = occurs(c, aid, b)
          case occurs {
            True -> {
              panic as "recursive type"
            }
            False -> {
              set_type_var(c, ref, Bound(b))
            }
          }
        }
      }
    a, VariableType(_) -> unify(c, b, a)
    NamedType(aname, amodule, _), NamedType(bname, bmodule, _)
      if aname != bname || amodule != bmodule
    -> {
      io.debug(#(aname, amodule))
      io.debug(#(bname, bmodule))
      panic as "failed to unify types"
    }
    NamedType(_, _, aargs), NamedType(_, _, bargs) -> {
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          panic as "incorrect number of type arguments"
        }
      }
    }
    FunctionType(aargs, aret), FunctionType(bargs, bret) -> {
      let c = unify(c, aret, bret)
      case list.strict_zip(aargs, bargs) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          panic as "incorrect number of function arguments"
        }
      }
    }
    TupleType(aelements), TupleType(belements) -> {
      case list.strict_zip(aelements, belements) {
        Ok(args) -> list.fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
        Error(_) -> {
          panic as "incorrect tuple size"
        }
      }
    }
    _, _ -> {
      io.debug(a)
      io.debug(b)
      panic as "failed to unify types"
    }
  }
}

fn occurs(c: Context, id: Int, in: Type) -> #(Context, Bool) {
  case in {
    VariableType(ref) ->
      case get_type_var(c, ref) {
        Bound(t) -> occurs(c, id, t)
        Unbound(i) -> {
          // TODO not sure if this "set" is needed
          let c = set_type_var(c, ref, Unbound(i))
          #(c, id == i)
        }
      }
    NamedType(_, _, args) ->
      list.fold(args, #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, arg)
        #(c, b || b1)
      })
    FunctionType(args, ret) ->
      list.fold([ret, ..args], #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, arg)
        #(c, b || b1)
      })
    TupleType(elements) ->
      list.fold(elements, #(c, False), fn(acc, arg) {
        let #(c, b) = acc
        let #(c, b1) = occurs(c, id, arg)
        #(c, b || b1)
      })
  }
}

/// follow any references to get the real type
fn resolve_type(c: Context, typ: Type) {
  case typ {
    VariableType(x) -> {
      let assert Ok(x) = env.get(c.type_vars, x)
      case x {
        Bound(x) -> resolve_type(c, x)
        Unbound(..) -> typ
      }
    }
    NamedType(..) -> typ
    FunctionType(..) -> typ
    TupleType(..) -> typ
  }
}
