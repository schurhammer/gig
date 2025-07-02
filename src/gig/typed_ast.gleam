import gig/call_graph
import gig/graph
import glance as g
import listx

import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/string

pub const builtin = "gleam"

pub const nil_type = NamedType("Nil", builtin, [])

pub const bool_type = NamedType("Bool", builtin, [])

pub const int_type = NamedType("Int", builtin, [])

pub const float_type = NamedType("Float", builtin, [])

pub const string_type = NamedType("String", builtin, [])

pub const bit_array_type = NamedType("BitArray", builtin, [])

pub type Ref {
  Ref(id: Int)
}

// TODO do we need unbound? what if unbound is just a missing key in the map
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

// TODO it would probably be good to parameterise the Type
// then we can offer an ast with resolved types and also library users
// can add their own info
pub type Module {
  Module(
    name: String,
    // TODO move these into context instead of module?
    // TODO put more info into the envs
    module_env: Dict(String, String),
    type_env: Dict(String, #(Poly, List(Variant))),
    value_env: Dict(String, ResolvedGlobal),
    imports: List(Definition(Import)),
    custom_types: List(Definition(CustomType)),
    type_aliases: List(Definition(TypeAlias)),
    constants: List(Definition(ConstantDefinition)),
    functions: List(Definition(FunctionDefinition)),
  )
}

pub type FunctionDefinition {
  FunctionDefinition(
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
  PatternConcatenate(
    typ: Type,
    prefix: String,
    prefix_name: Option(AssignmentName),
    suffix_name: AssignmentName,
  )
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
  Function(
    typ: Type,
    module: String,
    name: String,
    labels: List(Option(String)),
  )
  Constant(typ: Type, module: String, name: String, value: Expression)
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
    resolved_module: String,
    constructor: String,
    record: Expression,
    fields: List(#(String, Expression)),
    ordered_fields: List(Result(Expression, Type)),
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
    // TODO provide args in original order
    // arguments: List(Field(Expression)),
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
  BytesOption
  IntOption
  FloatOption
  BitsOption
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

pub type ConstantDefinition {
  ConstantDefinition(
    name: String,
    publicity: Publicity,
    annotation: Option(Annotation),
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
    typ: Poly,
    name: String,
    publicity: Publicity,
    parameters: List(String),
    aliased: Annotation,
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
  Dict(Ref, TypeVar)

pub type Context {
  Context(
    current_module: String,
    current_definition: String,
    type_vars: TypeVarEnv,
    modules: Dict(String, Module),
    type_uid: Int,
    temp_uid: Int,
  )
}

pub type LocalEnv =
  Dict(String, Type)

pub type TypeEnv =
  Dict(String, Type)

pub fn new_context() -> Context {
  Context(
    current_module: "",
    current_definition: "",
    type_vars: dict.new(),
    modules: dict.new(),
    type_uid: 0,
    temp_uid: 0,
  )
}

pub fn infer_module(
  c: Context,
  module: g.Module,
  current_module: String,
) -> Context {
  let modules =
    dict.insert(
      c.modules,
      current_module,
      Module(
        name: current_module,
        module_env: dict.new(),
        type_env: dict.new(),
        value_env: dict.new(),
        imports: [],
        custom_types: [],
        type_aliases: [],
        constants: [],
        functions: [],
      ),
    )

  let c = Context(..c, modules:, current_module:)

  // handle module imports
  let c =
    list.fold(module.imports, c, fn(c, imp) {
      update_module(c, fn(module) {
        let imp = imp.definition
        let module_id = imp.module

        let module_env = case imp.alias {
          Some(alias) ->
            case alias {
              g.Named(alias) -> dict.insert(module.module_env, alias, module_id)
              g.Discarded(_) -> module.module_env
            }
          None -> {
            let assert Ok(alias) = list.last(string.split(module_id, "/"))
            dict.insert(module.module_env, alias, module_id)
          }
        }

        let type_env =
          list.fold(imp.unqualified_types, module.type_env, fn(acc, imp) {
            let assert Ok(#(_, poly, variants)) =
              resolve_global_type_name(c, module_id, imp.name)
            let alias = case imp.alias {
              Some(alias) -> alias
              None -> imp.name
            }
            dict.insert(acc, alias, #(poly, variants))
          })

        let value_env =
          list.fold(imp.unqualified_values, module.value_env, fn(acc, imp) {
            let assert Ok(value) = resolve_global_name(c, module_id, imp.name)
            let alias = case imp.alias {
              Some(alias) -> alias
              None -> imp.name
            }
            dict.insert(acc, alias, value)
          })

        Module(..module, module_env:, type_env:, value_env:)
      })
    })

  // add types to env so they can reference eachother (but not yet constructors)
  let c =
    list.fold(module.custom_types, c, fn(c, def) {
      let custom = def.definition
      let c = Context(..c, current_definition: custom.name)

      let #(c, parameters) =
        list.fold(custom.parameters, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          let #(c, typ) = new_type_var_ref(c)
          #(c, [#(p, typ), ..l])
        })
      let parameters = list.reverse(parameters)
      let param_types = list.map(parameters, fn(x) { x.1 })
      let typ = NamedType(custom.name, c.current_module, param_types)
      let typ = generalise(c, typ)

      register_type(c, def.definition.name, typ, [])
    })

  // add types aliases to env so they can reference eachother
  let c =
    list.fold(module.type_aliases, c, fn(c, def) {
      let #(c, typ) = new_type_var_ref(c)
      register_type(c, def.definition.name, Poly([], typ), [])
    })

  // infer type aliases fr fr
  let #(c, aliases) =
    list.fold(module.type_aliases, #(c, []), fn(acc, def) {
      let #(c, aliases) = acc
      let c = Context(..c, current_definition: def.definition.name)

      // infer the alias type
      let #(c, alias) = infer_alias_type(c, def.definition)

      // update the placeholder type
      let assert Ok(#(_, placeholder, _)) =
        resolve_global_type_name(c, c.current_module, alias.name)
      let c = unify(c, alias.aliased.typ, placeholder.typ)

      #(c, [#(def, alias), ..aliases])
    })

  // create alias entries
  // we have to do this in two stages to make sure we genralize correctly
  let c =
    list.fold(aliases, c, fn(c, alias) {
      let #(def, alias) = alias
      let c = Context(..c, current_definition: alias.name)

      // create alias entry
      let poly = generalise(c, alias.aliased.typ)
      let c = register_type(c, alias.name, poly, [])
      let attrs = infer_attributes(c, def.attributes)
      let def = Definition(attrs, alias)
      update_module(c, fn(mod) {
        Module(..mod, type_aliases: [def, ..mod.type_aliases])
      })
    })

  // now infer custom types fr fr
  let c =
    list.fold(module.custom_types, c, fn(c, def) {
      let custom = def.definition
      let c = Context(..c, current_definition: custom.name)

      // reconstruct the type parameters
      let assert Ok(#(_, poly, _)) =
        resolve_global_type_name(c, c.current_module, custom.name)
      let param_types = list.map(poly.vars, fn(x) { VariableType(Ref(x)) })
      let parameters = list.zip(custom.parameters, param_types)

      // infer the custom type including variants
      let #(c, custom) = infer_custom_type(c, def.definition, parameters)
      let c = register_type(c, custom.name, custom.typ, custom.variants)
      let attrs = infer_attributes(c, def.attributes)
      let def = Definition(attrs, custom)
      update_module(c, fn(mod) {
        Module(..mod, custom_types: [def, ..mod.custom_types])
      })
    })

  let constants =
    call_graph.constant_graph(module)
    |> graph.strongly_connected_components()
    |> list.flatten()
    |> list.filter_map(fn(name) {
      module.constants
      |> list.find(fn(c) { c.definition.name == name })
    })

  // add functions to global env so they are available for recursion
  let c =
    list.fold(module.functions, c, fn(c, def) {
      let fun = def.definition
      let c = Context(..c, current_definition: fun.name)

      // create placeholder function type based on function signature
      let #(c, parameters, return) =
        infer_function_parameters(c, fun.parameters, fun.return)

      let #(c, return_type) = case return {
        Some(x) -> #(c, x.typ)
        None -> new_type_var_ref(c)
      }

      let param_types = list.map(parameters, fn(param) { param.typ })
      let param_labels = list.map(parameters, fn(f) { f.label })
      let typ = FunctionType(param_types, return_type)

      register_function(c, def.definition.name, Poly([], typ), param_labels)
    })

  // infer constant expressions
  let c =
    list.fold(constants, c, fn(c, def) {
      let #(c, constant) = infer_constant(c, def.definition)
      let c = Context(..c, current_definition: constant.name)

      let poly = generalise(c, constant.value.typ)
      let c = register_constant(c, constant.name, poly, constant.value)
      let attrs = infer_attributes(c, def.attributes)
      let def = Definition(attrs, constant)
      update_module(c, fn(mod) {
        Module(..mod, constants: [def, ..mod.constants])
      })
    })

  // create a function call graph to group mutually recursive functions
  // these will be type checked/inferred together as a group
  let rec_groups =
    call_graph.function_graph(module)
    |> graph.strongly_connected_components()

  list.fold(rec_groups, c, fn(c, group) {
    // find the function definitions by name
    let assert Ok(group) =
      list.try_map(group, fn(fun_name) {
        list.find(module.functions, fn(f) { f.definition.name == fun_name })
      })

    // infer types for the group
    let #(c, group) =
      list.fold(group, #(c, []), fn(acc, def) {
        let #(c, group) = acc
        let c = Context(..c, current_definition: def.definition.name)

        // infer function
        let #(c, fun) = infer_function(c, def.definition)
        let attrs = infer_attributes(c, def.attributes)
        let def = Definition(attrs, fun)

        #(c, [def, ..group])
      })

    // generalise
    list.fold(group, c, fn(c, def) {
      let fun = def.definition

      // unify placeholder type
      let assert Ok(placeholder) =
        resolve_global_name(c, c.current_module, fun.name)
      let c = unify(c, placeholder.typ.typ, fun.typ.typ)

      // generalise
      let typ = generalise(c, fun.typ.typ)
      let fun = FunctionDefinition(..fun, typ:)
      let def = Definition(..def, definition: fun)

      // update context
      let labels = list.map(fun.parameters, fn(f) { f.label })
      let c = register_function(c, fun.name, fun.typ, labels)
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

fn get_current_module(c: Context) -> Module {
  get_module(c, c.current_module)
}

fn get_module(c: Context, module: String) -> Module {
  let assert Ok(module) = dict.get(c.modules, module)
  module
}

fn update_module(c: Context, fun: fn(Module) -> Module) {
  let module = get_current_module(c)
  let module = fun(module)
  let modules = dict.insert(c.modules, c.current_module, module)
  Context(..c, modules:)
}

fn register_function(
  c: Context,
  name: String,
  typ: Poly,
  labels: List(Option(String)),
) -> Context {
  update_module(c, fn(module) {
    let value_env =
      dict.insert(
        module.value_env,
        name,
        FunctionGlobal(c.current_module, name, typ, labels),
      )
    Module(..module, value_env:)
  })
}

fn register_constant(
  c: Context,
  name: String,
  typ: Poly,
  value: Expression,
) -> Context {
  update_module(c, fn(module) {
    let value_env =
      dict.insert(
        module.value_env,
        name,
        ConstantGlobal(c.current_module, name, typ, value),
      )
    Module(..module, value_env:)
  })
}

fn register_type(
  c: Context,
  name: String,
  typ: Poly,
  variants: List(Variant),
) -> Context {
  update_module(c, fn(module) {
    let type_env = dict.insert(module.type_env, name, #(typ, variants))
    Module(..module, type_env:)
  })
}

fn infer_attributes(c: Context, attrs: List(g.Attribute)) {
  let attr_env =
    dict.new()
    |> dict.insert("c", nil_type)
    |> dict.insert("erlang", nil_type)
    |> dict.insert("javascript", nil_type)

  let #(_, attrs) =
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

fn infer_constant(c: Context, con: g.Constant) -> #(Context, ConstantDefinition) {
  let assert Ok(#(c, value)) = infer_expression(c, dict.new(), con.value)

  let publicity = case con.publicity {
    g.Public -> Public
    g.Private -> Private
  }

  let #(c, annotation) = case con.annotation {
    Some(anno) -> {
      let #(c, anno) = do_infer_annotation(c, dict.new(), anno)
      #(c, Some(anno))
    }
    None -> #(c, None)
  }

  let constant = ConstantDefinition(con.name, publicity, annotation, value)

  #(c, constant)
}

fn infer_function(c: Context, fun: g.Function) -> #(Context, FunctionDefinition) {
  let #(c, parameters, return) =
    infer_function_parameters(c, fun.parameters, fun.return)

  let #(c, return_type) = case return {
    Some(x) -> #(c, x.typ)
    None -> new_type_var_ref(c)
  }

  // put params into local env
  let n =
    list.fold(parameters, dict.new(), fn(n, param) {
      case param.name {
        Named(name) -> dict.insert(n, name, param.typ)
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

  let typ = Poly([], typ)

  let fun =
    FunctionDefinition(
      typ:,
      name:,
      publicity:,
      parameters:,
      return:,
      body:,
      location:,
    )
  #(c, fun)
}

fn infer_alias_type(c: Context, alias: g.TypeAlias) -> #(Context, TypeAlias) {
  let publicity = case alias.publicity {
    g.Public -> Public
    g.Private -> Private
  }

  let parameters = alias.parameters

  // create an env for the type variables
  let #(c, type_env, args) =
    list.fold(parameters, #(c, dict.new(), []), fn(acc, name) {
      let #(c, n, args) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = dict.insert(n, name, typ)
      let assert VariableType(ref) = typ
      #(c, n, [ref.id, ..args])
    })

  let args = list.reverse(args)

  let #(c, aliased) = do_infer_annotation(c, type_env, alias.aliased)

  let poly = Poly(args, aliased.typ)

  let alias = TypeAlias(poly, alias.name, publicity, parameters, aliased)

  #(c, alias)
}

fn infer_custom_type(
  c: Context,
  custom: g.CustomType,
  parameters: List(#(String, Type)),
) {
  // create a type variable for each parameter
  // these will be used when a field references a type parameter
  let param_types = list.map(parameters, fn(x) { x.1 })
  let module = c.current_module
  let name = custom.name
  let typ = NamedType(module:, name:, parameters: param_types)

  // create an env for param types
  let n =
    list.fold(parameters, dict.new(), fn(n, p) { dict.insert(n, p.0, p.1) })

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
      let label = case field {
        g.LabelledVariantField(_, label) -> Some(label)
        g.UnlabelledVariantField(_) -> None
      }
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

  let c = register_function(c, variant.name, typ, labels)

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
    list.fold(vars, #(c, dict.new()), fn(acc, name) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = dict.insert(n, name, typ)
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

      // instantiate the polymorphic type with the parameter types
      let assert Ok(#(_, poly, _variants)) =
        resolve_type_name(c, anno_module, name)
      let param_types = list.map(params, fn(param) { param.typ })
      let assert Ok(mapping) = list.strict_zip(poly.vars, param_types)
      let mapping = dict.from_list(mapping)
      let typ = do_instantiate(c, mapping, poly.typ)

      // let typ = NamedType(name, module, list.map(params, fn(x) { x.typ }))
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
      let assert Ok(typ) = dict.get(n, name)
      #(c, VariableAnno(typ, name))
    }
    g.HoleType(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      #(c, HoleAnno(typ, name))
    }
  }
}

pub type ResolvedGlobal {
  FunctionGlobal(
    module: String,
    name: String,
    typ: Poly,
    labels: List(Option(String)),
  )
  ConstantGlobal(module: String, name: String, typ: Poly, value: Expression)
}

type ResolvedVariable {
  ResolvedLocal(name: String, typ: Type)
  ResolvedGlobal(global: ResolvedGlobal)
}

fn resolve_unqualified_name(
  c: Context,
  n: LocalEnv,
  name: String,
) -> Result(ResolvedVariable, String) {
  case dict.get(n, name) {
    // try local env
    Ok(typ) -> Ok(ResolvedLocal(name, typ))
    Error(_) ->
      case resolve_unqualified_global(c, name) {
        Ok(value) -> Ok(ResolvedGlobal(value))
        Error(e) -> Error(e)
      }
  }
}

fn resolve_unqualified_global(
  c: Context,
  name: String,
) -> Result(ResolvedGlobal, String) {
  // try global env
  case resolve_global_name(c, c.current_module, name) {
    Ok(value) -> Ok(value)
    // try prelude
    Error(_) -> {
      case resolve_global_name(c, builtin, name) {
        Ok(value) -> Ok(value)
        Error(_) -> Error("Could not resolve name " <> name)
      }
    }
  }
}

fn resolve_aliased_name(
  c: Context,
  name: QualifiedName,
) -> Result(ResolvedGlobal, String) {
  case dict.get(get_current_module(c).module_env, name.module) {
    Ok(module) -> resolve_global_name(c, module, name.name)
    Error(_) -> Error("Could not resolve module name " <> name.module)
  }
}

pub fn resolve_global_name(
  c: Context,
  module_name: String,
  name: String,
) -> Result(ResolvedGlobal, String) {
  case dict.get(c.modules, module_name) {
    Ok(module) ->
      case dict.get(module.value_env, name) {
        Ok(value) -> Ok(value)
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
  case dict.get(get_current_module(c).module_env, module) {
    Ok(module) -> resolve_global_type_name(c, module, name)
    Error(_) -> Error("Could not resolve module name " <> module)
  }
}

pub fn resolve_global_type_name(
  c: Context,
  module: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), String) {
  case dict.get(c.modules, module) {
    Ok(c) ->
      case dict.get(c.type_env, name) {
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
    }
    None -> {
      resolve_unqualified_global(c, name)
    }
  }
}

fn new_temp_var(c: Context) -> #(Context, String) {
  let id = "T" <> int.to_string(c.temp_uid)
  #(Context(..c, temp_uid: c.temp_uid + 1), id)
}

fn new_type_var_ref(c: Context) {
  let ref = Ref(c.type_uid)
  let type_vars = dict.insert(c.type_vars, ref, Unbound(c.type_uid))
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
    g.PatternFloat(value) -> #(c, n, PatternFloat(float_type, value))
    g.PatternString(value) -> #(c, n, PatternString(string_type, value))
    g.PatternDiscard(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      #(c, n, PatternDiscard(typ, name))
    }
    g.PatternVariable(name) -> {
      let #(c, typ) = new_type_var_ref(c)
      let pattern = PatternVariable(typ, name)
      let n = dict.insert(n, name, typ)
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
      let n = dict.insert(n, name, pattern.typ)

      #(c, n, pattern)
    }
    g.PatternConcatenate(prefix, prefix_name, suffix_name) -> {
      // Add prefix_name to the environment if applicable
      let #(n, prefix_name) = case prefix_name {
        Some(g.Named(name)) -> {
          let n = dict.insert(n, name, string_type)
          #(n, Some(Named(name)))
        }
        Some(g.Discarded(name)) -> #(n, Some(Discarded(name)))
        None -> #(n, None)
      }

      // Add suffix_name to the environment if applicable
      let #(n, suffix_name) = case suffix_name {
        g.Named(name) -> {
          let n = dict.insert(n, name, string_type)
          #(n, Named(name))
        }
        g.Discarded(name) -> #(n, Discarded(name))
      }

      let pattern =
        PatternConcatenate(string_type, prefix, prefix_name, suffix_name)

      #(c, n, pattern)
    }
    g.PatternBitString(segs) -> {
      let #(c, n, segs) =
        list.fold(segs, #(c, n, []), fn(acc, seg) {
          let #(c, n, segs) = acc
          // TODO handle options
          // TODO unify type of pattern according to options
          let #(pattern, options) = seg

          let #(c, n, options, typ) =
            list.fold(options, #(c, n, [], None), fn(acc, option) {
              let #(c, n, options, typ) = acc
              // TODO handle all options
              let #(c, n, option, option_type) = case option {
                g.BigOption -> todo
                g.BytesOption -> #(c, n, BytesOption, Some(bit_array_type))
                g.BitsOption -> #(c, n, BitsOption, Some(bit_array_type))
                g.FloatOption -> todo
                g.IntOption -> todo
                g.LittleOption -> todo
                g.NativeOption -> todo
                g.SignedOption -> todo
                g.SizeOption(size) -> #(c, n, SizeOption(size), None)
                g.SizeValueOption(e) -> {
                  // TODO should this be infer_expression???
                  let #(c, n, e) = infer_pattern(c, n, e)
                  let c = unify(c, e.typ, int_type)
                  #(c, n, SizeValueOption(e), None)
                }
                g.UnitOption(_) -> todo
                g.UnsignedOption -> todo
                g.Utf16CodepointOption -> todo
                g.Utf16Option -> todo
                g.Utf32CodepointOption -> todo
                g.Utf32Option -> todo
                g.Utf8CodepointOption -> todo
                g.Utf8Option -> #(c, n, Utf8Option, Some(string_type))
              }
              let typ = case typ, option_type {
                Some(_), Some(_) -> panic as "type set twice"
                Some(_), None -> typ
                _, _ -> option_type
              }
              #(c, n, [option, ..options], typ)
            })
          let options = list.reverse(options)

          let #(c, n, pattern) = infer_pattern(c, n, pattern)
          #(c, n, [#(pattern, options), ..segs])
        })
      let segs = list.reverse(segs)
      #(c, n, PatternBitString(bit_array_type, segs))
    }
    g.PatternConstructor(module, constructor, arguments, with_spread) -> {
      // was a module name provided
      let with_module = case module {
        Some(_) -> True
        None -> False
      }

      // resolve the constructor function
      let assert Ok(FunctionGlobal(module, constructor, poly, labels)) =
        resolve_constructor_name(c, module, constructor)

      // infer the type of all arguments
      let #(c, n, arguments) =
        list.fold(arguments, #(c, n, []), fn(acc, arg) {
          let #(c, n, arguments) = acc

          let #(item, label) = case arg {
            g.LabelledField(label, item) -> #(item, Some(label))
            g.ShorthandField(label) -> #(g.PatternVariable(label), Some(label))
            g.UnlabelledField(item) -> #(item, None)
          }

          let #(c, n, arg2) = infer_pattern(c, n, item)
          #(c, n, [Field(label, arg2), ..arguments])
        })
      let arguments = list.reverse(arguments)

      // handle labels
      let #(c, ordered_arguments) = case with_spread {
        True -> {
          let #(c, args) =
            match_labels_optional(arguments, labels)
            |> list.fold(#(c, []), fn(acc, opt) {
              let #(c, opts) = acc
              let #(c, opt) = case opt {
                Some(opt) -> #(c, opt)
                None -> {
                  let #(c, typ) = new_type_var_ref(c)
                  #(c, PatternDiscard(typ, ""))
                }
              }
              #(c, [opt, ..opts])
            })
          #(c, list.reverse(args))
        }
        False -> {
          let args = match_labels(arguments, labels)
          #(c, args)
        }
      }
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
    list.fold(vars, #(c, dict.new()), fn(acc, name) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = dict.insert(n, name, typ)
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
          // infer value before binding the new variable
          let assert Ok(#(c, value)) = infer_expression(c, n, value)

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
          // TODO infer without desugaring
          case fun {
            g.Call(fun, args) -> {
              let params =
                list.index_map(pats, fn(_pat, i) {
                  g.FnParameter(g.Named("P" <> int.to_string(i)), None)
                })
              let body =
                list.index_fold(pats, xs, fn(body, pat, i) {
                  let param = g.Variable("P" <> int.to_string(i))
                  let assignment = g.Assignment(g.Let, pat, None, param)
                  [assignment, ..body]
                })
              let callback = g.Fn(params, None, body)
              let assert Ok(#(_, ifun)) = infer_expression(c, n, fun)
              let field = case ifun {
                Function(labels:, ..) ->
                  case list.last(labels) {
                    Ok(Some(label)) -> g.LabelledField(label, callback)
                    _ -> g.UnlabelledField(callback)
                  }
                _ -> g.UnlabelledField(callback)
              }
              let call = g.Call(fun, list.append(args, [field]))
              let assert Ok(#(c, exp)) = infer_expression(c, n, call)
              let statement = Expression(exp.typ, exp)
              #(c, [statement])
            }
            _ -> panic as "expected a function call"
          }
        }
      }
  }
}

fn match_labels(args: List(Field(a)), params: List(Option(String))) -> List(a) {
  // find the labels in the order specified by parameters
  // either we find the matching label or default to the first unlabelled arg
  case params {
    [] ->
      case args {
        [] -> []
        _ -> panic as "too many arguments"
      }
    [p, ..p_rest] ->
      case listx.pop(args, fn(a) { a.label == p }) {
        Ok(#(a, a_rest)) -> [a.item, ..match_labels(a_rest, p_rest)]
        Error(_) ->
          case listx.pop(args, fn(a) { a.label == None }) {
            Ok(#(a, a_rest)) -> [a.item, ..match_labels(a_rest, p_rest)]
            Error(_) -> panic as "no matching label"
          }
      }
  }
}

fn match_labels_optional(
  args: List(Field(a)),
  params: List(Option(String)),
) -> List(Option(a)) {
  // find the labels in the order specified by parameters
  case params {
    [] -> []
    [p, ..p_rest] ->
      case listx.pop(args, fn(a) { a.label == p }) {
        Ok(#(a, a_rest)) -> [
          Some(a.item),
          ..match_labels_optional(a_rest, p_rest)
        ]
        Error(_) -> [None, ..match_labels_optional(args, p_rest)]
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
        Ok(ResolvedGlobal(global)) ->
          case global {
            FunctionGlobal(module, name, typ, labels) -> {
              let #(c, typ) = instantiate(c, typ)
              Ok(#(c, Function(typ, module, name, labels)))
            }
            ConstantGlobal(module, name, typ, value) -> {
              let #(c, typ) = instantiate(c, typ)
              Ok(#(c, Constant(typ, module, name, value)))
            }
          }
        Ok(ResolvedLocal(name, typ)) -> {
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
      infer_fn(c, n, parameters, return, body, None)
    }
    g.RecordUpdate(module, constructor, expression, fields) -> {
      // Infer the type of the base record expression
      use #(c, base_expr) <- try(infer_expression(c, n, expression))

      // Resolve the constructor type
      let assert Ok(FunctionGlobal(res_module, constructor, poly, labels)) =
        resolve_constructor_name(c, module, constructor)

      // Instantiate the constructor type
      let #(c, constructor_type) = instantiate(c, poly)
      let assert FunctionType(constructor_args, constructor_ret) =
        constructor_type

      // Unify the base expression type with the constructor type
      let c = unify(c, base_expr.typ, constructor_ret)

      // Infer types for all updated fields
      use #(c, updated_fields) <- try(
        list.try_fold(fields, #(c, []), fn(acc, field) {
          let #(c, updated_fields) = acc
          let item = case field.item {
            Some(item) -> item
            None -> g.Variable(field.label)
          }
          use #(c, value) <- try(infer_expression(c, n, item))
          Ok(#(c, [#(field.label, value), ..updated_fields]))
        }),
      )
      let updated_fields = list.reverse(updated_fields)

      let fields = list.map(updated_fields, fn(x) { Field(Some(x.0), x.1) })
      let ordered_fields = match_labels_optional(fields, labels)
      let assert Ok(ordered_fields) =
        list.strict_zip(ordered_fields, constructor_args)
      let #(c, ordered_fields) =
        ordered_fields
        |> list.fold_right(#(c, []), fn(acc, x) {
          let #(c, fields) = acc
          let #(given, expected) = x
          let #(c, result) = case given {
            Some(e) -> {
              let c = unify(c, e.typ, expected)
              #(c, Ok(e))
            }
            None -> #(c, Error(expected))
          }
          #(c, [result, ..fields])
        })

      // The result type is the same as the constructor type
      let typ = constructor_ret

      // Create the RecordUpdate expression
      let record_update =
        RecordUpdate(
          typ: typ,
          module: module,
          resolved_module: res_module,
          constructor: constructor,
          record: base_expr,
          fields: updated_fields,
          ordered_fields: ordered_fields,
        )

      Ok(#(c, record_update))
    }
    g.FieldAccess(value, label) -> {
      let field_access = {
        // try to infer the value, otherwise it might be a module access
        use #(c, value) <- try(infer_expression(c, n, value))

        // field access must be on a named type
        let value_typ = case resolve_type(c, value.typ) {
          NamedType(type_name, module, _) -> Ok(#(type_name, module))
          _ -> Error("Field access attempted on invalid type. ")
        }
        use #(type_name, module) <- try(value_typ)

        // find the custom type definition
        let assert Ok(mod) = dict.get(c.modules, module)
        let assert Ok(custom) =
          list.find(mod.custom_types, fn(x) { x.definition.name == type_name })

        // access only works with one variant
        let variant = case custom.definition.variants {
          // TODO proper implementation checking all variants
          [variant, ..] -> Ok(variant)
          _ -> Error("Field access attempted on type with multiple variants.")
        }
        use variant <- try(variant)

        // find the matching field and index
        let field =
          variant.fields
          |> list.index_map(fn(x, i) { #(x, i) })
          |> list.find(fn(x) { { x.0 }.label == Some(label) })
          |> result.replace_error("Variant does not have a matching field.")
        use #(field, index) <- try(field)

        // create a getter function type
        let getter = FunctionType([custom.definition.typ.typ], field.item.typ)
        let getter = Poly(custom.definition.typ.vars, getter)
        let #(c, getter) = instantiate(c, getter)

        // unify the getter as if we're calling it on the value
        let #(c, typ) = new_type_var_ref(c)
        let c = unify(c, getter, FunctionType([value.typ], typ))

        Ok(#(c, FieldAccess(typ, value, module, variant.name, label, index)))
      }
      case field_access {
        Ok(access) -> Ok(access)
        Error(e) -> {
          // try a module access instead
          case value {
            g.Variable(module) -> {
              case resolve_aliased_name(c, QualifiedName(module, label)) {
                Ok(FunctionGlobal(module, name, poly, labels)) -> {
                  let #(c, typ) = instantiate(c, poly)
                  Ok(#(c, Function(typ, module, name, labels)))
                }
                Ok(ConstantGlobal(module, name, poly, value)) -> {
                  let #(c, typ) = instantiate(c, poly)
                  Ok(#(c, Constant(typ, module, name, value)))
                }
                Error(_) -> Error(e)
              }
            }
            _ -> Error(e)
          }
        }
      }
    }
    g.Call(fun, args) -> {
      // infer the type of the function
      use #(c, fun) <- try(infer_expression(c, n, fun))

      // handle labels
      let labels = case fun {
        Function(labels:, ..) -> labels
        _ -> list.map(args, fn(_) { None })
      }

      let args =
        list.map(args, fn(arg) {
          let #(label, arg) = case arg {
            g.LabelledField(label, item) -> #(Some(label), item)
            g.ShorthandField(label) -> #(Some(label), g.Variable(label))
            g.UnlabelledField(item) -> #(None, item)
          }
          Field(label, arg)
        })

      let args = match_labels(args, labels)

      // use fun parameter type as type hints for inferring arguments
      let args = case resolve_type(c, fun.typ) {
        FunctionType(params, _ret) -> {
          let params = list.map(params, Some)
          let assert Ok(args) = list.strict_zip(params, args)
          args
        }
        _ -> list.map(args, fn(arg) { #(None, arg) })
      }
      // infer the type of all args
      use #(c, args) <- try(
        list.try_fold(args, #(c, []), fn(acc, item) {
          let #(hint, arg) = item
          let #(c, args) = acc

          // give type hint when arg is a fn
          let result = case arg {
            g.Fn(parameters, return, body) ->
              infer_fn(c, n, parameters, return, body, hint)
            _ -> infer_expression(c, n, arg)
          }
          use #(c, arg) <- try(result)

          let c = case hint {
            Some(hint) -> unify(c, hint, arg.typ)
            None -> c
          }
          Ok(#(c, [arg, ..args]))
        }),
      )
      let args = list.reverse(args)
      let arg_types = list.map(args, fn(x) { x.typ })
      // unify the function type with the types of args
      let #(c, typ) = new_type_var_ref(c)
      let c = unify(c, fun.typ, FunctionType(arg_types, typ))
      Ok(#(c, Call(typ, fun, args)))
    }
    g.TupleIndex(tuple, index) -> {
      use #(c, tuple) <- try(infer_expression(c, n, tuple))
      case resolve_type(c, tuple.typ) {
        TupleType(elements) -> {
          let typ = tuple_index_type(c, elements, index)
          Ok(#(c, TupleIndex(typ, tuple, index)))
        }
        _ -> Error("tuple index on non-tuple")
      }
    }
    g.FnCapture(label, fun, before, after) -> {
      // TODO return non-desugared version
      let #(c, x) = new_temp_var(c)
      let arg = case label {
        Some(label) -> g.LabelledField(label, g.Variable(x))
        None -> g.UnlabelledField(g.Variable(x))
      }
      let args = list.flatten([before, [arg], after])
      let param = g.FnParameter(g.Named(x), None)
      let abs = g.Fn([param], None, [g.Expression(g.Call(fun, args))])
      infer_expression(c, n, abs)
    }
    g.BitString(segs) -> {
      use #(c, segs) <- try(
        list.try_fold(segs, #(c, []), fn(acc, seg) {
          let #(c, segs) = acc
          let #(expression, options) = seg
          let #(c, options, typ) =
            list.fold(options, #(c, [], None), fn(acc, option) {
              let #(c, options, typ) = acc
              // TODO handle all options
              let #(c, option, option_type) = case option {
                g.BigOption -> todo
                g.BytesOption -> #(c, BytesOption, Some(bit_array_type))
                g.BitsOption -> #(c, BitsOption, Some(bit_array_type))
                g.FloatOption -> todo
                g.IntOption -> todo
                g.LittleOption -> todo
                g.NativeOption -> todo
                g.SignedOption -> todo
                g.SizeOption(size) -> #(c, SizeOption(size), None)
                g.SizeValueOption(e) -> {
                  let assert Ok(#(c, e)) = infer_expression(c, n, e)
                  let c = unify(c, e.typ, int_type)
                  #(c, SizeValueOption(e), None)
                }
                g.UnitOption(_) -> todo
                g.UnsignedOption -> todo
                g.Utf16CodepointOption -> todo
                g.Utf16Option -> todo
                g.Utf32CodepointOption -> todo
                g.Utf32Option -> todo
                g.Utf8CodepointOption -> todo
                g.Utf8Option -> {
                  #(c, Utf8Option, Some(string_type))
                }
              }
              let typ = case typ, option_type {
                Some(_), Some(_) -> panic as "type set twice"
                Some(_), None -> typ
                _, _ -> option_type
              }
              #(c, [option, ..options], typ)
            })
          let options = list.reverse(options)
          let typ = case typ {
            Some(typ) -> typ
            None -> int_type
          }
          use #(c, expression) <- try(infer_expression(c, n, expression))
          let c = unify(c, expression.typ, typ)
          Ok(#(c, [#(expression, options), ..segs]))
        }),
      )
      let segs = list.reverse(segs)
      Ok(#(c, BitString(bit_array_type, segs)))
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
            list.fold_right(clause.patterns, #(c, n, []), fn(acc, pat) {
              let #(c, n, pats) = acc

              // each pattern has a corresponding subject
              let assert Ok(sub_pats) = list.strict_zip(subjects, pat)
              let #(c, n, pat) =
                list.fold_right(sub_pats, #(c, n, []), fn(acc, sub_pat) {
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
    g.BinaryOperator(g.Pipe, left, right) -> {
      // TODO return a not-desugared version
      case right {
        g.Call(fun, args) ->
          infer_expression(c, n, g.Call(fun, [g.UnlabelledField(left), ..args]))
        g.Variable(_name) ->
          infer_expression(c, n, g.Call(right, [g.UnlabelledField(left)]))
        g.FieldAccess(_value, _field) ->
          infer_expression(c, n, g.Call(right, [g.UnlabelledField(left)]))
        g.FnCapture(label, fun, before, after) -> {
          let args = case label {
            Some(label) -> [before, [g.LabelledField(label, left)], after]
            None -> [before, [g.UnlabelledField(left)], after]
          }
          infer_expression(c, n, g.Call(fun, list.flatten(args)))
        }
        _ -> {
          io.debug(right)
          panic as "pipe to unexpected expression"
        }
      }
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
        g.Pipe -> panic as "pipe should be handeled elsewhere"

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

fn infer_fn(
  c: Context,
  n: Dict(String, Type),
  parameters: List(g.FnParameter),
  return: Option(g.Type),
  body: List(g.Statement),
  hint: Option(Type),
) -> Result(#(Context, Expression), String) {
  // map parameters to FunctionParameter for code reuse
  let parameters =
    list.map(parameters, fn(p) { g.FunctionParameter(None, p.name, p.type_) })

  let #(c, parameters, return) =
    infer_function_parameters(c, parameters, return)

  let #(c, return_type) = case return {
    Some(x) -> #(c, x.typ)
    None -> new_type_var_ref(c)
  }

  // compute function type
  let parameter_types = list.map(parameters, fn(x) { x.typ })
  let typ = FunctionType(parameter_types, return_type)

  // unify parameters with type hint
  let c = case hint {
    Some(hint) -> unify(c, typ, hint)
    None -> c
  }

  // put params into local env
  let n =
    list.fold(parameters, n, fn(n, param) {
      case param.name {
        Named(name) -> dict.insert(n, name, param.typ)
        Discarded(_) -> n
      }
    })

  // infer body
  let #(c, body) = infer_body(c, n, body)

  // unify the return type with the last statement
  let c = case list.last(body) {
    Ok(statement) -> unify(c, return_type, statement.typ)
    Error(_) -> c
  }

  let fun = Fn(typ:, parameters:, return:, body:)
  Ok(#(c, fun))
}

type PolyEnv =
  Dict(Int, Type)

fn get_type_var(c: Context, var: Ref) {
  let assert Ok(x) = dict.get(c.type_vars, var)
  x
}

fn set_type_var(c: Context, var: Ref, bind: TypeVar) {
  Context(..c, type_vars: dict.insert(c.type_vars, var, bind))
}

fn instantiate(c: Context, poly: Poly) -> #(Context, Type) {
  let #(c, n) =
    list.fold(poly.vars, #(c, dict.new()), fn(acc, var) {
      let #(c, n) = acc
      let #(c, new_var) = new_type_var_ref(c)
      let n = dict.insert(n, var, new_var)
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
          case dict.get(n, x) {
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
      let message =
        "failed to unify types in "
        <> c.current_module
        <> "."
        <> c.current_definition
      panic as message
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
pub fn resolve_type(c: Context, typ: Type) {
  case typ {
    VariableType(x) -> {
      let assert Ok(x) = dict.get(c.type_vars, x)
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

pub fn resolve_type_deep(c: Context, typ: Type) {
  case typ {
    VariableType(x) -> {
      let assert Ok(x) = dict.get(c.type_vars, x)
      case x {
        Bound(x) -> resolve_type_deep(c, x)
        Unbound(..) -> typ
      }
    }
    NamedType(name, mod, args) ->
      NamedType(name, mod, list.map(args, resolve_type_deep(c, _)))
    FunctionType(args, ret) ->
      FunctionType(
        list.map(args, resolve_type_deep(c, _)),
        resolve_type_deep(c, ret),
      )
    TupleType(elements) ->
      TupleType(list.map(elements, resolve_type_deep(c, _)))
  }
}

/// Convert a Type to a string representation
pub fn type_to_string(typ: Type) -> String {
  case typ {
    NamedType(name, "", []) -> name
    NamedType(name, "", parameters) ->
      name
      <> "("
      <> string.join(list.map(parameters, type_to_string), ", ")
      <> ")"
    NamedType(name, module, []) -> module <> "." <> name
    NamedType(name, module, parameters) ->
      module
      <> "."
      <> name
      <> "("
      <> string.join(list.map(parameters, type_to_string), ", ")
      <> ")"
    TupleType([]) -> "()"
    TupleType([single]) -> "(" <> type_to_string(single) <> ",)"
    TupleType(elements) ->
      "(" <> string.join(list.map(elements, type_to_string), ", ") <> ")"
    FunctionType([], return) -> "fn() -> " <> type_to_string(return)
    FunctionType(parameters, return) ->
      "fn("
      <> string.join(list.map(parameters, type_to_string), ", ")
      <> ") -> "
      <> type_to_string(return)
    VariableType(Ref(id)) -> "?" <> int.to_string(id)
  }
}

/// Convert a Poly to a string representation
pub fn poly_to_string(poly: Poly) -> String {
  case poly {
    Poly([], typ) -> type_to_string(typ)
    Poly(vars, typ) -> {
      let var_strings = list.map(vars, fn(var) { "?" <> int.to_string(var) })
      "forall " <> string.join(var_strings, " ") <> ". " <> type_to_string(typ)
    }
  }
}

/// Convert a Publicity to a string representation
pub fn publicity_to_string(publicity: Publicity) -> String {
  case publicity {
    Public -> "pub"
    Private -> ""
  }
}

/// Convert an AssignmentName to a string representation
pub fn assignment_name_to_string(name: AssignmentName) -> String {
  case name {
    Named(value) -> value
    Discarded(value) -> value
  }
}

/// Convert an Annotation to a string representation
pub fn annotation_to_string(annotation: Annotation) -> String {
  case annotation {
    NamedAnno(typ, name, None, parameters) ->
      name
      <> case parameters {
        [] -> ""
        _ ->
          "("
          <> string.join(list.map(parameters, annotation_to_string), ", ")
          <> ")"
      }
      <> " : "
      <> type_to_string(typ)
    NamedAnno(typ, name, Some(module), parameters) ->
      module
      <> "."
      <> name
      <> case parameters {
        [] -> ""
        _ ->
          "("
          <> string.join(list.map(parameters, annotation_to_string), ", ")
          <> ")"
      }
      <> " : "
      <> type_to_string(typ)
    TupleAnno(typ, elements) ->
      "("
      <> string.join(list.map(elements, annotation_to_string), ", ")
      <> ")"
      <> " : "
      <> type_to_string(typ)
    FunctionAnno(typ, parameters, return) ->
      "fn("
      <> string.join(list.map(parameters, annotation_to_string), ", ")
      <> ") -> "
      <> annotation_to_string(return)
      <> " : "
      <> type_to_string(typ)
    VariableAnno(typ, name) -> name <> " : " <> type_to_string(typ)
    HoleAnno(typ, name) -> name <> " : " <> type_to_string(typ)
  }
}

/// Convert a FunctionParameter to a string representation
pub fn function_parameter_to_string(param: FunctionParameter) -> String {
  let FunctionParameter(typ, label, name, annotation) = param
  let label_str = case label {
    Some(l) -> l <> " "
    None -> ""
  }
  let name_str = assignment_name_to_string(name)
  let annotation_str = case annotation {
    Some(anno) -> " " <> annotation_to_string(anno)
    None -> ""
  }
  label_str <> name_str <> ": " <> type_to_string(typ) <> annotation_str
}

/// Convert a Span to a string representation
pub fn span_to_string(span: Span) -> String {
  let Span(start, end) = span
  "@" <> int.to_string(start) <> "-" <> int.to_string(end)
}

/// Convert a Statement to a string representation (simplified)
pub fn statement_to_string(statement: Statement) -> String {
  case statement {
    Use(typ, patterns, function) ->
      "use "
      <> string.join(list.map(patterns, pattern_to_string), ", ")
      <> " <- "
      <> expression_to_string(function)
      <> " : "
      <> type_to_string(typ)
    Assignment(typ, kind, pattern, annotation, value) -> {
      let kind_str = case kind {
        Let -> "let"
        Assert -> "assert"
      }
      let annotation_str = case annotation {
        Some(anno) -> ": " <> annotation_to_string(anno)
        None -> ""
      }
      kind_str
      <> " "
      <> pattern_to_string(pattern)
      <> annotation_str
      <> " = "
      <> expression_to_string(value)
      <> " : "
      <> type_to_string(typ)
    }
    Expression(typ, expression) ->
      expression_to_string(expression) <> " : " <> type_to_string(typ)
  }
}

/// Convert a Pattern to a string representation (simplified)
pub fn pattern_to_string(pattern: Pattern) -> String {
  case pattern {
    PatternInt(typ, value) -> value <> " : " <> type_to_string(typ)
    PatternFloat(typ, value) -> value <> " : " <> type_to_string(typ)
    PatternString(typ, value) ->
      "\"" <> value <> "\"" <> " : " <> type_to_string(typ)
    PatternDiscard(typ, name) -> name <> " : " <> type_to_string(typ)
    PatternVariable(typ, name) -> name <> " : " <> type_to_string(typ)
    PatternTuple(typ, elems) ->
      "("
      <> string.join(list.map(elems, pattern_to_string), ", ")
      <> ")"
      <> " : "
      <> type_to_string(typ)
    PatternList(typ, elements, tail) -> {
      let elements_str =
        string.join(list.map(elements, pattern_to_string), ", ")
      let tail_str = case tail {
        Some(t) -> ", .." <> pattern_to_string(t)
        None -> ""
      }
      "[" <> elements_str <> tail_str <> "]" <> " : " <> type_to_string(typ)
    }
    PatternAssignment(typ, pattern, name) ->
      pattern_to_string(pattern)
      <> " as "
      <> name
      <> " : "
      <> type_to_string(typ)
    PatternConcatenate(typ, prefix, prefix_name, suffix_name) -> {
      let prefix_name_str = case prefix_name {
        Some(name) -> assignment_name_to_string(name)
        None -> "_"
      }
      "<<"
      <> prefix
      <> " as "
      <> prefix_name_str
      <> ", rest as "
      <> assignment_name_to_string(suffix_name)
      <> ">>"
      <> " : "
      <> type_to_string(typ)
    }
    PatternBitString(typ, segments) ->
      "<<"
      <> string.join(list.map(segments, fn(_) { "..." }), ", ")
      <> ">>"
      <> " : "
      <> type_to_string(typ)
    PatternConstructor(typ, module, constructor, arguments, _, _, _) -> {
      let module_str = case module == "" {
        True -> ""
        False -> module <> "."
      }
      let args_str = case arguments {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(arguments, fn(arg) { pattern_to_string(arg.item) }),
            ", ",
          )
          <> ")"
      }
      module_str <> constructor <> args_str <> " : " <> type_to_string(typ)
    }
  }
}

/// Convert an Expression to a string representation (simplified)
pub fn expression_to_string(expression: Expression) -> String {
  case expression {
    Int(typ, value) -> value <> " : " <> type_to_string(typ)
    Float(typ, value) -> value <> " : " <> type_to_string(typ)
    String(typ, value) -> "\"" <> value <> "\"" <> " : " <> type_to_string(typ)
    LocalVariable(typ, name) -> name <> " : " <> type_to_string(typ)
    Function(typ, module, name, labels) -> {
      let labels_str = case labels {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(labels, fn(label) {
              case label {
                Some(l) -> l <> ": "
                None -> ""
              }
            }),
            ", ",
          )
          <> ")"
      }
      module <> "." <> name <> labels_str <> " : " <> type_to_string(typ)
    }
    Constant(typ, module, name, _) -> {
      module <> "." <> name <> " : " <> type_to_string(typ)
    }
    NegateInt(typ, value) ->
      "-" <> expression_to_string(value) <> " : " <> type_to_string(typ)
    NegateBool(typ, value) ->
      "!" <> expression_to_string(value) <> " : " <> type_to_string(typ)
    Block(typ, statements) ->
      "{\n  "
      <> string.join(list.map(statements, statement_to_string), "\n  ")
      <> "\n}"
      <> " : "
      <> type_to_string(typ)
    Panic(typ, value) -> {
      let value_str = case value {
        Some(v) -> expression_to_string(v)
        None -> ""
      }
      "panic(" <> value_str <> ")" <> " : " <> type_to_string(typ)
    }
    Todo(typ, value) -> {
      let value_str = case value {
        Some(v) -> expression_to_string(v)
        None -> ""
      }
      "todo(" <> value_str <> ")" <> " : " <> type_to_string(typ)
    }
    Tuple(typ, elements) ->
      "("
      <> string.join(list.map(elements, expression_to_string), ", ")
      <> ")"
      <> " : "
      <> type_to_string(typ)
    List(typ, elements, rest) -> {
      let elements_str =
        string.join(list.map(elements, expression_to_string), ", ")
      let rest_str = case rest {
        Some(r) -> ", .." <> expression_to_string(r)
        None -> ""
      }
      "[" <> elements_str <> rest_str <> "]" <> " : " <> type_to_string(typ)
    }
    Fn(typ, parameters, return, body) -> {
      let param_strings = list.map(parameters, function_parameter_to_string)
      let params_str = string.join(param_strings, ", ")
      let return_str = case return {
        Some(ret) -> " -> " <> annotation_to_string(ret)
        None -> ""
      }
      let body_str = case body {
        [] -> "{}"
        statements -> {
          "{\n    "
          <> string.join(list.map(statements, statement_to_string), "\n    ")
          <> "\n  }"
        }
      }
      "fn("
      <> params_str
      <> ")"
      <> return_str
      <> " "
      <> body_str
      <> " : "
      <> type_to_string(typ)
    }
    RecordUpdate(typ, module, resolved_module, constructor, record, fields, _) -> {
      let module_str = case module {
        Some(m) -> m <> "."
        None -> ""
      }
      let fields_str =
        string.join(
          list.map(fields, fn(field) {
            let #(label, value) = field
            label <> ": " <> expression_to_string(value)
          }),
          ", ",
        )
      module_str
      <> constructor
      <> "("
      <> expression_to_string(record)
      <> " with "
      <> fields_str
      <> ")"
      <> " : "
      <> type_to_string(typ)
    }
    FieldAccess(typ, container, module, variant, label, index) -> {
      expression_to_string(container)
      <> "."
      <> label
      <> " : "
      <> type_to_string(typ)
    }
    Call(typ, function, ordered_arguments) -> {
      let args_str =
        string.join(list.map(ordered_arguments, expression_to_string), ", ")
      expression_to_string(function)
      <> "("
      <> args_str
      <> ")"
      <> " : "
      <> type_to_string(typ)
    }
    TupleIndex(typ, tuple, index) -> {
      expression_to_string(tuple)
      <> "."
      <> int.to_string(index)
      <> " : "
      <> type_to_string(typ)
    }
    FnCapture(typ, label, function, arguments_before, arguments_after) -> {
      let before_str =
        string.join(
          list.map(arguments_before, fn(field) {
            let Field(label, item) = field
            let label_str = case label {
              Some(l) -> l <> ": "
              None -> ""
            }
            label_str <> expression_to_string(item)
          }),
          ", ",
        )
      let after_str =
        string.join(
          list.map(arguments_after, fn(field) {
            let Field(label, item) = field
            let label_str = case label {
              Some(l) -> l <> ": "
              None -> ""
            }
            label_str <> expression_to_string(item)
          }),
          ", ",
        )
      let label_str = case label {
        Some(l) -> l <> ": "
        None -> ""
      }
      let args = case before_str == "" && after_str == "" {
        True -> label_str <> "_"
        False ->
          before_str
          <> case before_str == "" {
            True -> ""
            False -> ", "
          }
          <> label_str
          <> "_"
          <> case after_str == "" {
            True -> ""
            False -> ", " <> after_str
          }
      }
      expression_to_string(function)
      <> "("
      <> args
      <> ")"
      <> " : "
      <> type_to_string(typ)
    }
    BitString(typ, segments) -> {
      let segments_str =
        string.join(
          list.map(segments, fn(segment) {
            let #(expr, _options) = segment
            expression_to_string(expr)
          }),
          ", ",
        )
      "<<" <> segments_str <> ">>" <> " : " <> type_to_string(typ)
    }
    Case(typ, subjects, clauses) -> {
      let subjects_str =
        string.join(list.map(subjects, expression_to_string), ", ")
      let clauses_str =
        string.join(
          list.map(clauses, fn(clause) {
            let Clause(patterns, guard, body) = clause
            let patterns_str =
              string.join(
                list.map(patterns, fn(pattern_list) {
                  string.join(list.map(pattern_list, pattern_to_string), ", ")
                }),
                " | ",
              )
            let guard_str = case guard {
              Some(g) -> " if " <> expression_to_string(g)
              None -> ""
            }
            patterns_str <> guard_str <> " -> " <> expression_to_string(body)
          }),
          "\n    ",
        )
      "case "
      <> subjects_str
      <> " {\n    "
      <> clauses_str
      <> "\n  }"
      <> " : "
      <> type_to_string(typ)
    }
    BinaryOperator(typ, name, left, right) -> {
      expression_to_string(left)
      <> " "
      <> binary_operator_to_string(name)
      <> " "
      <> expression_to_string(right)
      <> " : "
      <> type_to_string(typ)
    }
  }
}

/// Convert a BinaryOperator to string representation
fn binary_operator_to_string(op: g.BinaryOperator) -> String {
  case op {
    g.And -> "&&"
    g.Or -> "||"
    g.LtInt -> "<"
    g.LtEqInt -> "<="
    g.LtFloat -> "<."
    g.LtEqFloat -> "<=."
    g.Eq -> "=="
    g.NotEq -> "!="
    g.GtEqInt -> ">="
    g.GtInt -> ">"
    g.GtEqFloat -> ">=."
    g.GtFloat -> ">."
    g.AddInt -> "+"
    g.AddFloat -> "+."
    g.SubInt -> "-"
    g.SubFloat -> "-."
    g.MultInt -> "*"
    g.MultFloat -> "*."
    g.DivInt -> "/"
    g.DivFloat -> "/."
    g.RemainderInt -> "%"
    g.Concatenate -> "<>"
    g.Pipe -> "|>"
  }
}

/// Convert a FunctionDefinition to a string representation
pub fn function_definition_to_string(func_def: FunctionDefinition) -> String {
  let FunctionDefinition(
    typ,
    name,
    publicity,
    parameters,
    return,
    body,
    location,
  ) = func_def

  let publicity_str = case publicity_to_string(publicity) {
    "" -> ""
    pub_str -> pub_str <> " "
  }

  let param_strings = list.map(parameters, function_parameter_to_string)
  let params_str = string.join(param_strings, ", ")

  let return_str = case return {
    Some(ret) -> " -> " <> annotation_to_string(ret)
    None -> ""
  }

  let body_str = case body {
    [] -> "{}"
    statements -> {
      "{\n  "
      <> string.join(list.map(statements, statement_to_string), "\n  ")
      <> "\n}"
    }
  }

  let location_str = " " <> span_to_string(location)

  publicity_str
  <> "fn "
  <> name
  <> "("
  <> params_str
  <> ")"
  <> return_str
  <> " : "
  <> poly_to_string(typ)
  <> " "
  <> body_str
  <> location_str
}

/// Convert a Type to a string representation with resolved type variables
pub fn type_to_string_resolved(c: Context, typ: Type) -> String {
  let resolved_typ = resolve_type(c, typ)
  case resolved_typ {
    NamedType(name, "", []) -> name
    NamedType(name, "", parameters) ->
      name
      <> "("
      <> string.join(list.map(parameters, type_to_string_resolved(c, _)), ", ")
      <> ")"
    NamedType(name, module, []) -> module <> "." <> name
    NamedType(name, module, parameters) ->
      module
      <> "."
      <> name
      <> "("
      <> string.join(list.map(parameters, type_to_string_resolved(c, _)), ", ")
      <> ")"
    TupleType([]) -> "()"
    TupleType([single]) -> "(" <> type_to_string_resolved(c, single) <> ",)"
    TupleType(elements) ->
      "("
      <> string.join(list.map(elements, type_to_string_resolved(c, _)), ", ")
      <> ")"
    FunctionType([], return) -> "fn() -> " <> type_to_string_resolved(c, return)
    FunctionType(parameters, return) ->
      "fn("
      <> string.join(list.map(parameters, type_to_string_resolved(c, _)), ", ")
      <> ") -> "
      <> type_to_string_resolved(c, return)
    VariableType(Ref(id)) -> "?" <> int.to_string(id)
  }
}

/// Convert a Poly to a string representation with resolved type variables
pub fn poly_to_string_resolved(c: Context, poly: Poly) -> String {
  case poly {
    Poly([], typ) -> type_to_string_resolved(c, typ)
    Poly(vars, typ) -> {
      let var_strings = list.map(vars, fn(var) { "?" <> int.to_string(var) })
      "forall "
      <> string.join(var_strings, " ")
      <> ". "
      <> type_to_string_resolved(c, typ)
    }
  }
}

/// Convert an Annotation to a string representation with resolved type variables
pub fn annotation_to_string_resolved(
  c: Context,
  annotation: Annotation,
) -> String {
  case annotation {
    NamedAnno(typ, name, None, parameters) ->
      name
      <> case parameters {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(parameters, annotation_to_string_resolved(c, _)),
            ", ",
          )
          <> ")"
      }
      <> " : "
      <> type_to_string_resolved(c, typ)
    NamedAnno(typ, name, Some(module), parameters) ->
      module
      <> "."
      <> name
      <> case parameters {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(parameters, annotation_to_string_resolved(c, _)),
            ", ",
          )
          <> ")"
      }
      <> " : "
      <> type_to_string_resolved(c, typ)
    TupleAnno(typ, elements) ->
      "("
      <> string.join(
        list.map(elements, annotation_to_string_resolved(c, _)),
        ", ",
      )
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    FunctionAnno(typ, parameters, return) ->
      "fn("
      <> string.join(
        list.map(parameters, annotation_to_string_resolved(c, _)),
        ", ",
      )
      <> ") -> "
      <> annotation_to_string_resolved(c, return)
      <> " : "
      <> type_to_string_resolved(c, typ)
    VariableAnno(typ, name) -> name <> " : " <> type_to_string_resolved(c, typ)
    HoleAnno(typ, name) -> name <> " : " <> type_to_string_resolved(c, typ)
  }
}

/// Convert a FunctionParameter to a string representation with resolved type variables
pub fn function_parameter_to_string_resolved(
  c: Context,
  param: FunctionParameter,
) -> String {
  let FunctionParameter(typ, label, name, annotation) = param
  let label_str = case label {
    Some(l) -> l <> " "
    None -> ""
  }
  let name_str = assignment_name_to_string(name)
  let annotation_str = case annotation {
    Some(anno) -> " " <> annotation_to_string_resolved(c, anno)
    None -> ""
  }
  label_str
  <> name_str
  <> ": "
  <> type_to_string_resolved(c, typ)
  <> annotation_str
}

/// Convert a Statement to a string representation with resolved type variables
pub fn statement_to_string_resolved(c: Context, statement: Statement) -> String {
  case statement {
    Use(typ, patterns, function) ->
      "use "
      <> string.join(list.map(patterns, pattern_to_string_resolved(c, _)), ", ")
      <> " <- "
      <> expression_to_string_resolved(c, function)
      <> " : "
      <> type_to_string_resolved(c, typ)
    Assignment(typ, kind, pattern, annotation, value) -> {
      let kind_str = case kind {
        Let -> "let"
        Assert -> "assert"
      }
      let annotation_str = case annotation {
        Some(anno) -> ": " <> annotation_to_string_resolved(c, anno)
        None -> ""
      }
      kind_str
      <> " "
      <> pattern_to_string_resolved(c, pattern)
      <> annotation_str
      <> " = "
      <> expression_to_string_resolved(c, value)
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    Expression(typ, expression) ->
      expression_to_string_resolved(c, expression)
      <> " : "
      <> type_to_string_resolved(c, typ)
  }
}

/// Convert a Pattern to a string representation with resolved type variables
pub fn pattern_to_string_resolved(c: Context, pattern: Pattern) -> String {
  case pattern {
    PatternInt(typ, value) -> value <> " : " <> type_to_string_resolved(c, typ)
    PatternFloat(typ, value) ->
      value <> " : " <> type_to_string_resolved(c, typ)
    PatternString(typ, value) ->
      "\"" <> value <> "\"" <> " : " <> type_to_string_resolved(c, typ)
    PatternDiscard(typ, name) ->
      name <> " : " <> type_to_string_resolved(c, typ)
    PatternVariable(typ, name) ->
      name <> " : " <> type_to_string_resolved(c, typ)
    PatternTuple(typ, elems) ->
      "("
      <> string.join(list.map(elems, pattern_to_string_resolved(c, _)), ", ")
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    PatternList(typ, elements, tail) -> {
      let elements_str =
        string.join(list.map(elements, pattern_to_string_resolved(c, _)), ", ")
      let tail_str = case tail {
        Some(t) -> ", .." <> pattern_to_string_resolved(c, t)
        None -> ""
      }
      "["
      <> elements_str
      <> tail_str
      <> "]"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    PatternAssignment(typ, pattern, name) ->
      pattern_to_string_resolved(c, pattern)
      <> " as "
      <> name
      <> " : "
      <> type_to_string_resolved(c, typ)
    PatternConcatenate(typ, prefix, prefix_name, suffix_name) -> {
      let prefix_name_str = case prefix_name {
        Some(name) -> assignment_name_to_string(name)
        None -> "_"
      }
      "<<"
      <> prefix
      <> " as "
      <> prefix_name_str
      <> ", rest as "
      <> assignment_name_to_string(suffix_name)
      <> ">>"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    PatternBitString(typ, segments) ->
      "<<"
      <> string.join(list.map(segments, fn(_) { "..." }), ", ")
      <> ">>"
      <> " : "
      <> type_to_string_resolved(c, typ)
    PatternConstructor(typ, module, constructor, arguments, _, _, _) -> {
      let module_str = case module == "" {
        True -> ""
        False -> module <> "."
      }
      let args_str = case arguments {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(arguments, fn(arg) {
              pattern_to_string_resolved(c, arg.item)
            }),
            ", ",
          )
          <> ")"
      }
      module_str
      <> constructor
      <> args_str
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
  }
}

/// Convert an Expression to a string representation with resolved type variables
pub fn expression_to_string_resolved(
  c: Context,
  expression: Expression,
) -> String {
  case expression {
    Int(typ, value) -> value <> " : " <> type_to_string_resolved(c, typ)
    Float(typ, value) -> value <> " : " <> type_to_string_resolved(c, typ)
    String(typ, value) ->
      "\"" <> value <> "\"" <> " : " <> type_to_string_resolved(c, typ)
    LocalVariable(typ, name) -> name <> " : " <> type_to_string_resolved(c, typ)
    Function(typ, module, name, labels) -> {
      let labels_str = case labels {
        [] -> ""
        _ ->
          "("
          <> string.join(
            list.map(labels, fn(label) {
              case label {
                Some(l) -> l <> ": "
                None -> ""
              }
            }),
            ", ",
          )
          <> ")"
      }
      module
      <> "."
      <> name
      <> labels_str
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    Constant(typ, module, name, _) -> {
      module <> "." <> name <> " : " <> type_to_string_resolved(c, typ)
    }
    NegateInt(typ, value) ->
      "-"
      <> expression_to_string_resolved(c, value)
      <> " : "
      <> type_to_string_resolved(c, typ)
    NegateBool(typ, value) ->
      "!"
      <> expression_to_string_resolved(c, value)
      <> " : "
      <> type_to_string_resolved(c, typ)
    Block(typ, statements) ->
      "{\n  "
      <> string.join(
        list.map(statements, statement_to_string_resolved(c, _)),
        "\n  ",
      )
      <> "\n}"
      <> " : "
      <> type_to_string_resolved(c, typ)
    Panic(typ, value) -> {
      let value_str = case value {
        Some(v) -> expression_to_string_resolved(c, v)
        None -> ""
      }
      "panic(" <> value_str <> ")" <> " : " <> type_to_string_resolved(c, typ)
    }
    Todo(typ, value) -> {
      let value_str = case value {
        Some(v) -> expression_to_string_resolved(c, v)
        None -> ""
      }
      "todo(" <> value_str <> ")" <> " : " <> type_to_string_resolved(c, typ)
    }
    Tuple(typ, elements) ->
      "("
      <> string.join(
        list.map(elements, expression_to_string_resolved(c, _)),
        ", ",
      )
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    List(typ, elements, rest) -> {
      let elements_str =
        string.join(
          list.map(elements, expression_to_string_resolved(c, _)),
          ", ",
        )
      let rest_str = case rest {
        Some(r) -> ", .." <> expression_to_string_resolved(c, r)
        None -> ""
      }
      "["
      <> elements_str
      <> rest_str
      <> "]"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    Fn(typ, parameters, return, body) -> {
      let param_strings =
        list.map(parameters, function_parameter_to_string_resolved(c, _))
      let params_str = string.join(param_strings, ", ")
      let return_str = case return {
        Some(ret) -> " -> " <> annotation_to_string_resolved(c, ret)
        None -> ""
      }
      let body_str = case body {
        [] -> "{}"
        statements -> {
          "{\n    "
          <> string.join(
            list.map(statements, statement_to_string_resolved(c, _)),
            "\n    ",
          )
          <> "\n  }"
        }
      }
      "fn("
      <> params_str
      <> ")"
      <> return_str
      <> " "
      <> body_str
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    RecordUpdate(typ, module, resolved_module, constructor, record, fields, _) -> {
      let module_str = case module {
        Some(m) -> m <> "."
        None -> ""
      }
      let fields_str =
        string.join(
          list.map(fields, fn(field) {
            let #(label, value) = field
            label <> ": " <> expression_to_string_resolved(c, value)
          }),
          ", ",
        )
      module_str
      <> constructor
      <> "("
      <> expression_to_string_resolved(c, record)
      <> " with "
      <> fields_str
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    FieldAccess(typ, container, module, variant, label, index) -> {
      expression_to_string_resolved(c, container)
      <> "."
      <> label
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    Call(typ, function, ordered_arguments) -> {
      let args_str =
        string.join(
          list.map(ordered_arguments, expression_to_string_resolved(c, _)),
          ", ",
        )
      expression_to_string_resolved(c, function)
      <> "("
      <> args_str
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    TupleIndex(typ, tuple, index) -> {
      expression_to_string_resolved(c, tuple)
      <> "."
      <> int.to_string(index)
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    FnCapture(typ, label, function, arguments_before, arguments_after) -> {
      let before_str =
        string.join(
          list.map(arguments_before, fn(field) {
            let Field(label, item) = field
            let label_str = case label {
              Some(l) -> l <> ": "
              None -> ""
            }
            label_str <> expression_to_string_resolved(c, item)
          }),
          ", ",
        )
      let after_str =
        string.join(
          list.map(arguments_after, fn(field) {
            let Field(label, item) = field
            let label_str = case label {
              Some(l) -> l <> ": "
              None -> ""
            }
            label_str <> expression_to_string_resolved(c, item)
          }),
          ", ",
        )
      let label_str = case label {
        Some(l) -> l <> ": "
        None -> ""
      }
      let args = case before_str == "" && after_str == "" {
        True -> label_str <> "_"
        False ->
          before_str
          <> case before_str == "" {
            True -> ""
            False -> ", "
          }
          <> label_str
          <> "_"
          <> case after_str == "" {
            True -> ""
            False -> ", " <> after_str
          }
      }
      expression_to_string_resolved(c, function)
      <> "("
      <> args
      <> ")"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    BitString(typ, segments) -> {
      let segments_str =
        string.join(
          list.map(segments, fn(segment) {
            let #(expr, _options) = segment
            expression_to_string_resolved(c, expr)
          }),
          ", ",
        )
      "<<" <> segments_str <> ">>" <> " : " <> type_to_string_resolved(c, typ)
    }
    Case(typ, subjects, clauses) -> {
      let subjects_str =
        string.join(
          list.map(subjects, expression_to_string_resolved(c, _)),
          ", ",
        )
      let clauses_str =
        string.join(
          list.map(clauses, fn(clause) {
            let Clause(patterns, guard, body) = clause
            let patterns_str =
              string.join(
                list.map(patterns, fn(pattern_list) {
                  string.join(
                    list.map(pattern_list, pattern_to_string_resolved(c, _)),
                    ", ",
                  )
                }),
                " | ",
              )
            let guard_str = case guard {
              Some(g) -> " if " <> expression_to_string_resolved(c, g)
              None -> ""
            }
            patterns_str
            <> guard_str
            <> " -> "
            <> expression_to_string_resolved(c, body)
          }),
          "\n    ",
        )
      "case "
      <> subjects_str
      <> " {\n    "
      <> clauses_str
      <> "\n  }"
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
    BinaryOperator(typ, name, left, right) -> {
      expression_to_string_resolved(c, left)
      <> " "
      <> binary_operator_to_string(name)
      <> " "
      <> expression_to_string_resolved(c, right)
      <> " : "
      <> type_to_string_resolved(c, typ)
    }
  }
}

/// Convert a FunctionDefinition to a string representation with resolved type variables
pub fn function_definition_to_string_resolved(
  c: Context,
  func_def: FunctionDefinition,
) -> String {
  let FunctionDefinition(
    typ,
    name,
    publicity,
    parameters,
    return,
    body,
    location,
  ) = func_def

  let publicity_str = case publicity_to_string(publicity) {
    "" -> ""
    pub_str -> pub_str <> " "
  }

  let param_strings =
    list.map(parameters, function_parameter_to_string_resolved(c, _))
  let params_str = string.join(param_strings, ", ")

  let return_str = case return {
    Some(ret) -> " -> " <> annotation_to_string_resolved(c, ret)
    None -> ""
  }

  let body_str = case body {
    [] -> "{}"
    statements -> {
      "{\n  "
      <> string.join(
        list.map(statements, statement_to_string_resolved(c, _)),
        "\n  ",
      )
      <> "\n}"
    }
  }

  let location_str = " " <> span_to_string(location)

  publicity_str
  <> "fn "
  <> name
  <> "("
  <> params_str
  <> ")"
  <> return_str
  <> " : "
  <> poly_to_string_resolved(c, typ)
  <> " "
  <> body_str
  <> location_str
}
