import gig/call_graph
import gig/graph
import glance.{Span} as g
import listx

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const builtin = "gleam"

pub const nil_type = NamedType("Nil", builtin, [])

pub const bool_type = NamedType("Bool", builtin, [])

pub const int_type = NamedType("Int", builtin, [])

pub const codepoint_type = NamedType("UtfCodepoint", builtin, [])

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

pub type Span =
  g.Span

pub type Location {
  Location(module: String, definition: String, span: Span)
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
  Assert(typ: Type, expression: Expression, message: Option(Expression))
  Expression(typ: Type, expression: Expression)
}

pub type AssignmentKind {
  Let
  LetAssert
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
    ordered_arguments: List(Field(Pattern)),
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
  Echo(typ: Type, value: Option(Expression))
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
    ordered_fields: List(Result(Field(Expression), Type)),
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

// TODO: errors should probably get a Span
pub type Error {
  UnresolvedModule(location: Location, name: String)
  UnresolvedGlobal(location: Location, name: String)
  UnresolvedType(location: Location, name: String)
  UnresolvedFunction(location: Location, name: String)
  EmptyBlock(location: Location)
  InvalidTupleAccess(location: Location)
  InvalidFieldAccess(location: Location)
  FieldNotFound(location: Location, name: String)
  UnresolvedTypeVariable(location: Location, name: String)
  NotAFunction(location: Location, name: String)
  WrongArity(location: Location, expected_arg_count: Int, actual_arg_count: Int)
  LabelNotFound(location: Location, name: String)
  TupleIndexOutOfBounds(location: Location, tuple_size: Int, index: Int)
  IncompatibleTypes(location: Location, type_a: Type, type_b: Type)
  RecursiveTypeError(location: Location)
  BitPatternSegmentTypeOverSpecified(location: Location)
}

pub type QualifiedName {
  QualifiedName(module: String, name: String)
}

pub type Context {
  Context(
    module_source: String,
    current_module: String,
    current_definition: String,
    current_span: Span,
    type_vars: Dict(Ref, TypeVar),
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
    module_source: "",
    current_module: "",
    current_definition: "",
    current_span: Span(0, 0),
    type_vars: dict.new(),
    modules: dict.new(),
    type_uid: 0,
    temp_uid: 0,
  )
}

pub fn infer_module(
  c: Context,
  module: g.Module,
  module_name: String,
  module_source: String,
) -> Result(Context, Error) {
  let modules =
    dict.insert(
      c.modules,
      module_name,
      Module(
        name: module_name,
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

  let c = Context(..c, modules:, current_module: module_name, module_source:)

  // handle module imports
  use c <- result.try(
    list.try_fold(module.imports, c, fn(c, imp) {
      try_update_module(c, fn(module) {
        let imp = imp.definition
        let module_id = imp.module

        let module_env = case imp.alias {
          Some(alias) ->
            case alias {
              g.Named(alias) -> dict.insert(module.module_env, alias, module_id)
              g.Discarded(_) -> module.module_env
            }
          None -> {
            // assert: imported name is a non-empty string
            let assert Ok(alias) = list.last(string.split(module_id, "/"))
            dict.insert(module.module_env, alias, module_id)
          }
        }

        use type_env <- result.try(
          list.try_fold(imp.unqualified_types, module.type_env, fn(acc, imp) {
            use #(_, poly, variants) <- result.map(resolve_global_type_name(
              c,
              module_id,
              imp.name,
            ))
            let alias = case imp.alias {
              Some(alias) -> alias
              None -> imp.name
            }
            dict.insert(acc, alias, #(poly, variants))
          }),
        )

        use value_env <- result.map(
          list.try_fold(imp.unqualified_values, module.value_env, fn(acc, imp) {
            use value <- result.map(resolve_global_name(c, module_id, imp.name))
            let alias = case imp.alias {
              Some(alias) -> alias
              None -> imp.name
            }
            dict.insert(acc, alias, value)
          }),
        )

        Module(..module, module_env:, type_env:, value_env:)
      })
    }),
  )

  // add types to env so they can reference eachother (but not yet constructors)
  let c =
    list.fold(module.custom_types, c, fn(c, def) {
      let custom = def.definition
      let c = Context(..c, current_definition: custom.name)
      let c = Context(..c, current_span: def.definition.location)

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
  use #(c, aliases) <- result.try(
    list.try_fold(module.type_aliases, #(c, []), fn(acc, def) {
      let #(c, aliases) = acc
      let c = Context(..c, current_definition: def.definition.name)
      let c = Context(..c, current_span: def.definition.location)

      // infer the alias type
      use #(c, alias) <- result.try(infer_alias_type(c, def.definition))

      // update the placeholder type
      use #(_, placeholder, _) <- result.try(resolve_global_type_name(
        c,
        c.current_module,
        alias.name,
      ))
      use c <- result.map(unify(c, alias.aliased.typ, placeholder.typ))

      #(c, [#(def, alias), ..aliases])
    }),
  )

  // create alias entries
  // we have to do this in two stages to make sure we genralize correctly
  use c <- result.try(
    list.try_fold(aliases, c, fn(c, alias) {
      let #(def, alias) = alias
      let c = Context(..c, current_definition: alias.name)
      let c = Context(..c, current_span: def.definition.location)

      // create alias entry
      let poly = generalise(c, alias.aliased.typ)
      let c = register_type(c, alias.name, poly, [])
      use attrs <- result.map(infer_attributes(c, def.attributes))
      let def = Definition(attrs, alias)
      update_module(c, fn(mod) {
        Module(..mod, type_aliases: [def, ..mod.type_aliases])
      })
    }),
  )

  // now infer custom types fr fr
  use c <- result.try(
    list.try_fold(module.custom_types, c, fn(c, def) {
      let custom = def.definition
      let c = Context(..c, current_definition: custom.name)
      let c = Context(..c, current_span: def.definition.location)

      // reconstruct the type parameters
      use #(_, poly, _) <- result.try(resolve_global_type_name(
        c,
        c.current_module,
        custom.name,
      ))
      let param_types = list.map(poly.vars, fn(x) { VariableType(Ref(x)) })
      let parameters = list.zip(custom.parameters, param_types)

      // infer the custom type including variants
      use #(c, custom) <- result.try(infer_custom_type(
        c,
        def.definition,
        parameters,
      ))
      let c = register_type(c, custom.name, custom.typ, custom.variants)
      use attrs <- result.map(infer_attributes(c, def.attributes))
      let def = Definition(attrs, custom)
      update_module(c, fn(mod) {
        Module(..mod, custom_types: [def, ..mod.custom_types])
      })
    }),
  )

  let constants =
    call_graph.constant_graph(module)
    |> graph.strongly_connected_components()
    |> list.flatten()
    |> list.filter_map(fn(name) {
      module.constants
      |> list.find(fn(c) { c.definition.name == name })
    })

  // add functions to global env so they are available for recursion
  use c <- result.try(
    list.try_fold(module.functions, c, fn(c, def) {
      let fun = def.definition
      let c = Context(..c, current_definition: fun.name)
      let c = Context(..c, current_span: def.definition.location)

      // create placeholder function type based on function signature
      use #(c, parameters, return) <- result.map(infer_function_parameters(
        c,
        fun.parameters,
        fun.return,
      ))

      let #(c, return_type) = case return {
        Some(x) -> #(c, x.typ)
        None -> new_type_var_ref(c)
      }

      let param_types = list.map(parameters, fn(param) { param.typ })
      let param_labels = list.map(parameters, fn(f) { f.label })
      let typ = FunctionType(param_types, return_type)

      register_function(c, def.definition.name, Poly([], typ), param_labels)
    }),
  )

  // infer constant expressions
  use c <- result.try(
    list.try_fold(constants, c, fn(c, def) {
      use #(c, constant) <- result.try(infer_constant(c, def.definition))
      let c = Context(..c, current_definition: constant.name)
      let c = Context(..c, current_span: def.definition.location)

      let poly = generalise(c, constant.value.typ)
      let c = register_constant(c, constant.name, poly, constant.value)
      use attrs <- result.map(infer_attributes(c, def.attributes))
      let def = Definition(attrs, constant)
      update_module(c, fn(mod) {
        Module(..mod, constants: [def, ..mod.constants])
      })
    }),
  )

  // create a function call graph to group mutually recursive functions
  // these will be type checked/inferred together as a group
  let rec_groups =
    call_graph.function_graph(module)
    |> graph.strongly_connected_components()

  list.try_fold(rec_groups, c, fn(c, group) {
    // find the function definitions by name
    use group <- result.try(
      list.try_map(group, fn(fun_name) {
        list.find(module.functions, fn(f) { f.definition.name == fun_name })
        |> result.replace_error(UnresolvedFunction(location(c), fun_name))
      }),
    )

    // infer types for the group
    use #(c, group) <- result.try(
      list.try_fold(group, #(c, []), fn(acc, def) {
        let #(c, group) = acc
        let c = Context(..c, current_definition: def.definition.name)
        let c = Context(..c, current_span: def.definition.location)

        // infer function
        use #(c, fun) <- result.try(infer_function(c, def.definition))
        use attrs <- result.map(infer_attributes(c, def.attributes))
        let def = Definition(attrs, fun)

        #(c, [def, ..group])
      }),
    )

    // generalise
    list.try_fold(group, c, fn(c, def) {
      let fun = def.definition

      // unify placeholder type
      use placeholder <- result.try(resolve_global_name(
        c,
        c.current_module,
        fun.name,
      ))
      use c <- result.map(unify(c, placeholder.typ.typ, fun.typ.typ))

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

/// Returns a human-readable string description of the error.
/// Does not include the span (location) of the error.
pub fn inspect_error(error: Error) {
  case error {
    UnresolvedModule(name:, ..) -> "Module with name '" <> name <> "' not found"
    UnresolvedGlobal(name:, ..) -> "Global with name '" <> name <> "' not found"
    UnresolvedType(name:, ..) -> "Type with name '" <> name <> "' not found"
    UnresolvedFunction(name:, ..) ->
      "Function with name '" <> name <> "' not found"
    EmptyBlock(..) -> "Block is empty"
    InvalidTupleAccess(..) -> "Attempted tuple access on a non-tuple type"
    InvalidFieldAccess(..) -> "Attempted field access on a non-record type"
    FieldNotFound(name:, ..) ->
      "This record does not have a field named '" <> name <> "'"
    UnresolvedTypeVariable(name:, ..) ->
      "Type variable with name '" <> name <> "' not found"
    NotAFunction(name:, ..) -> "The variable '" <> name <> "' is not a function"
    WrongArity(expected_arg_count:, actual_arg_count:, ..) ->
      "Function with arity "
      <> int.to_string(expected_arg_count)
      <> " called with "
      <> int.to_string(actual_arg_count)
      <> " arguments"
    LabelNotFound(name:, ..) ->
      "The called function does not have an argument with label '"
      <> name
      <> "'"
    TupleIndexOutOfBounds(tuple_size:, index:, ..) ->
      "Tuple index "
      <> int.to_string(index)
      <> " exceeds the size of the tuple ("
      <> int.to_string(tuple_size)
      <> ")"
    IncompatibleTypes(type_a:, type_b:, ..) ->
      "Incompatible types: a = "
      <> string.inspect(type_a)
      <> ", b = "
      <> string.inspect(type_b)
    RecursiveTypeError(..) ->
      "Encountered a cyclical dependency between type variables"
    BitPatternSegmentTypeOverSpecified(_) ->
      "Bit pattern segment type set multiple times"
  }
}

fn generalise(c: Context, typ: Type) {
  let tvs =
    list.unique(find_tvs(c, typ))
    |> list.sort(int.compare)
  Poly(tvs, typ)
}

fn get_current_module(c: Context) -> Module {
  // assert: the current module exists
  let assert Ok(module) = dict.get(c.modules, c.current_module)
  module
}

fn update_module(c: Context, fun: fn(Module) -> Module) {
  let module = get_current_module(c)
  let module = fun(module)
  let modules = dict.insert(c.modules, c.current_module, module)
  Context(..c, modules:)
}

fn try_update_module(c: Context, fun: fn(Module) -> Result(Module, Error)) {
  let module = get_current_module(c)
  use module <- result.map(fun(module))
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

  use #(_, attrs) <- result.map(
    list.try_fold(attrs, #(c, []), fn(acc, attr) {
      let #(c, attrs) = acc
      use #(c, exprs) <- result.map(
        list.try_fold(attr.arguments, #(c, []), fn(acc, attr) {
          let #(c, exprs) = acc
          use #(c, expr) <- result.map(infer_expression(c, attr_env, attr))
          #(c, [expr, ..exprs])
        }),
      )
      let exprs = list.reverse(exprs)
      let attr = Attribute(attr.name, exprs)
      #(c, [attr, ..attrs])
    }),
  )
  list.reverse(attrs)
}

fn infer_constant(
  c: Context,
  con: g.Constant,
) -> Result(#(Context, ConstantDefinition), Error) {
  use #(c, value) <- result.try(infer_expression(c, dict.new(), con.value))

  let publicity = case con.publicity {
    g.Public -> Public
    g.Private -> Private
  }

  use #(c, annotation) <- result.map(case con.annotation {
    Some(anno) -> {
      use #(c, anno) <- result.map(do_infer_annotation(c, dict.new(), anno))
      #(c, Some(anno))
    }
    None -> Ok(#(c, None))
  })

  let constant = ConstantDefinition(con.name, publicity, annotation, value)

  #(c, constant)
}

fn infer_function(
  c: Context,
  fun: g.Function,
) -> Result(#(Context, FunctionDefinition), Error) {
  use #(c, parameters, return) <- result.try(infer_function_parameters(
    c,
    fun.parameters,
    fun.return,
  ))

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
  use #(c, body) <- result.try(infer_body(c, n, fun.body))

  // compute function type
  let parameter_types = list.map(parameters, fn(x) { x.typ })
  let typ = FunctionType(parameter_types, return_type)

  // unify the return type with the last statement
  use c <- result.map(case list.last(body) {
    Ok(statement) -> unify(c, return_type, statement.typ)
    Error(_) -> Ok(c)
  })

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

fn infer_alias_type(
  c: Context,
  alias: g.TypeAlias,
) -> Result(#(Context, TypeAlias), Error) {
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
      // TODO: does this need wrapping in a result or is it guaranteed to succeed?
      let assert VariableType(ref) = typ
      #(c, n, [ref.id, ..args])
    })

  let args = list.reverse(args)

  use #(c, aliased) <- result.map(do_infer_annotation(
    c,
    type_env,
    alias.aliased,
  ))

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
  use #(c, variants) <- result.map(
    list.try_fold(custom.variants, #(c, []), fn(acc, variant) {
      let #(c, l) = acc
      use #(c, v) <- result.map(infer_variant(c, n, typ, variant))
      #(c, [v, ..l])
    }),
  )
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

fn infer_variant(
  c,
  n,
  typ: Type,
  variant: g.Variant,
) -> Result(#(Context, Variant), Error) {
  use #(c, fields) <- result.map(
    list.try_fold(variant.fields, #(c, []), fn(acc, field) {
      let #(c, fields) = acc
      use #(c, annotation) <- result.map(do_infer_annotation(c, n, field.item))
      let label = case field {
        g.LabelledVariantField(_, label) -> Some(label)
        g.UnlabelledVariantField(_) -> None
      }
      let field = Field(label, annotation)
      #(c, [field, ..fields])
    }),
  )
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
    g.NamedType(_, _name, _module, parameters) ->
      list.flat_map(parameters, find_vars_in_type)
    g.TupleType(_, elements) -> list.flat_map(elements, find_vars_in_type)
    g.FunctionType(_, parameters, return) ->
      list.flat_map([return, ..parameters], find_vars_in_type)
    g.VariableType(_, name) -> [name]
    g.HoleType(_, _) -> []
  }
}

fn infer_function_parameters(
  c: Context,
  parameters: List(g.FunctionParameter),
  return: Option(g.Type),
) -> Result(#(Context, List(FunctionParameter), Option(Annotation)), Error) {
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

  let vars = list.unique(vars)

  // create an env for the type variables
  let #(c, type_env) =
    list.fold(vars, #(c, dict.new()), fn(acc, name) {
      let #(c, n) = acc
      let #(c, typ) = new_type_var_ref(c)
      let n = dict.insert(n, name, typ)
      #(c, n)
    })

  // create type vars for parameters
  use #(c, params) <- result.try(
    list.try_fold(parameters, #(c, []), fn(acc, param) {
      let #(c, param_types) = acc

      let label = param.label

      let name = case param.name {
        g.Named(s) -> Named(s)
        g.Discarded(s) -> Discarded(s)
      }

      use #(c, annotation) <- result.map(case param.type_ {
        Some(typ) -> {
          use #(c, anno) <- result.map(do_infer_annotation(c, type_env, typ))
          #(c, Some(anno))
        }
        None -> Ok(#(c, None))
      })

      let #(c, typ) = case annotation {
        Some(a) -> #(c, a.typ)
        None -> new_type_var_ref(c)
      }

      #(c, [FunctionParameter(typ, label, name, annotation), ..param_types])
    }),
  )
  let params = list.reverse(params)

  // handle function return type
  use #(c, return) <- result.map(case return {
    Some(typ) -> {
      use #(c, anno) <- result.map(do_infer_annotation(c, type_env, typ))
      #(c, Some(anno))
    }
    None -> Ok(#(c, None))
  })

  #(c, params, return)
}

fn do_infer_annotation(
  c: Context,
  n: TypeEnv,
  typ: g.Type,
) -> Result(#(Context, Annotation), Error) {
  case typ {
    g.NamedType(_, name, anno_module, params) -> {
      use #(c, params) <- result.try(
        list.try_fold(params, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          use #(c, p) <- result.map(do_infer_annotation(c, n, p))
          #(c, [p, ..l])
        }),
      )
      let params = list.reverse(params)

      // instantiate the polymorphic type with the parameter types
      use #(_, poly, _variants) <- result.try(resolve_type_name(
        c,
        anno_module,
        name,
      ))
      let param_types = list.map(params, fn(param) { param.typ })
      use mapping <- result.map(
        list.strict_zip(poly.vars, param_types)
        |> result.map_error(fn(_) {
          WrongArity(
            location(c),
            list.length(poly.vars),
            list.length(param_types),
          )
        })
        |> result.map(dict.from_list),
      )
      let typ = do_instantiate(c, mapping, poly.typ)

      // let typ = NamedType(name, module, list.map(params, fn(x) { x.typ }))
      #(c, NamedAnno(typ, name, anno_module, params))
    }
    g.TupleType(_, elements) -> {
      use #(c, elements) <- result.map(
        list.try_fold(elements, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          use #(c, p) <- result.map(do_infer_annotation(c, n, p))
          #(c, [p, ..l])
        }),
      )
      let elements = list.reverse(elements)
      let typ = TupleType(list.map(elements, fn(x) { x.typ }))
      #(c, TupleAnno(typ, elements))
    }
    g.FunctionType(_, parameters, return) -> {
      use #(c, params) <- result.try(
        list.try_fold(parameters, #(c, []), fn(acc, p) {
          let #(c, l) = acc
          use #(c, p) <- result.map(do_infer_annotation(c, n, p))
          #(c, [p, ..l])
        }),
      )
      let params = list.reverse(params)
      use #(c, ret) <- result.map(do_infer_annotation(c, n, return))
      let typ = FunctionType(list.map(params, fn(x) { x.typ }), ret.typ)
      #(c, FunctionAnno(typ, params, ret))
    }
    g.VariableType(_, name) -> {
      use typ <- result.map(
        dict.get(n, name)
        |> result.replace_error(UnresolvedTypeVariable(location(c), name)),
      )
      #(c, VariableAnno(typ, name))
    }
    g.HoleType(_, name) -> {
      let #(c, typ) = new_type_var_ref(c)
      Ok(#(c, HoleAnno(typ, name)))
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

/// Resolve an unqualified name against the local and then global environment.
fn resolve_unqualified_name(
  c: Context,
  n: LocalEnv,
  name: String,
) -> Result(ResolvedVariable, Error) {
  dict.get(n, name)
  |> result.map(ResolvedLocal(name, _))
  |> result.try_recover(fn(_) {
    resolve_unqualified_global(c, name) |> result.map(ResolvedGlobal)
  })
}

/// Resolve an unqualified name against the global environment.
fn resolve_unqualified_global(
  c: Context,
  name: String,
) -> Result(ResolvedGlobal, Error) {
  // try global env
  resolve_global_name(c, c.current_module, name)
  |> result.try_recover(fn(_) {
    // try prelude
    resolve_global_name(c, builtin, name)
  })
}

/// Resolve a global from a possibly aliased module
fn resolve_aliased_global(
  c: Context,
  name: QualifiedName,
) -> Result(ResolvedGlobal, Error) {
  resolve_module(c, name.module)
  |> result.try(resolve_global_name(c, _, name.name))
}

/// Resolve a name from the global environment
pub fn resolve_global_name(
  c: Context,
  module_name: String,
  name: String,
) -> Result(ResolvedGlobal, Error) {
  use module <- result.try(get_module(c, module_name))
  dict.get(module.value_env, name)
  |> result.replace_error(UnresolvedGlobal(location(c), name))
}

/// Resolve a type name from the global environment
fn resolve_type_name(
  c: Context,
  mod: Option(String),
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), Error) {
  case mod {
    Some(mod) -> resolve_aliased_type_name(c, mod, name)
    None ->
      resolve_global_type_name(c, c.current_module, name)
      |> result.try_recover(fn(_) { resolve_global_type_name(c, builtin, name) })
  }
}

/// Resolve a type name from a possibly aliased module 
fn resolve_aliased_type_name(
  c: Context,
  module: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), Error) {
  resolve_module(c, module)
  |> result.try(resolve_global_type_name(c, _, name))
}

// Resolve a type name from a fully qualified module name
pub fn resolve_global_type_name(
  c: Context,
  module_name: String,
  name: String,
) -> Result(#(QualifiedName, Poly, List(Variant)), Error) {
  use module <- result.try(get_module(c, module_name))
  use #(typ, variants) <- result.map(
    dict.get(module.type_env, name)
    |> result.replace_error(UnresolvedType(
      location(c),
      module_name <> "." <> name,
    )),
  )
  #(QualifiedName(module_name, name), typ, variants)
}

/// Resolve a qualified or unqualified contructor name
fn resolve_constructor_name(c: Context, mod: Option(String), name: String) {
  case mod {
    Some(mod) -> resolve_aliased_global(c, QualifiedName(mod, name))
    None -> resolve_unqualified_global(c, name)
  }
}

/// Resolve a module alias to its fully qualified name
fn resolve_module(c: Context, module_name: String) -> Result(String, Error) {
  dict.get(get_current_module(c).module_env, module_name)
  |> result.replace_error(UnresolvedModule(location(c), module_name))
}

/// Get a module by its fully qualified name
fn get_module(c: Context, module_name: String) -> Result(Module, Error) {
  dict.get(c.modules, module_name)
  |> result.replace_error(UnresolvedModule(location(c), module_name))
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
) -> Result(#(Context, LocalEnv, Pattern), Error) {
  case pattern {
    g.PatternInt(_, value) -> Ok(#(c, n, PatternInt(int_type, value)))
    g.PatternFloat(_, value) -> Ok(#(c, n, PatternFloat(float_type, value)))
    g.PatternString(_, value) -> Ok(#(c, n, PatternString(string_type, value)))
    g.PatternDiscard(_, name) -> {
      let #(c, typ) = new_type_var_ref(c)
      Ok(#(c, n, PatternDiscard(typ, name)))
    }
    g.PatternVariable(_, name) -> {
      let #(c, typ) = new_type_var_ref(c)
      let pattern = PatternVariable(typ, name)
      let n = dict.insert(n, name, typ)
      Ok(#(c, n, pattern))
    }
    g.PatternTuple(_, elements) -> {
      // Infer types for all elements in the tuple pattern
      use #(c, n, elems) <- result.map(
        list.try_fold(elements, #(c, n, []), fn(acc, elem) {
          let #(c, n, patterns) = acc
          use #(c, n, pattern) <- result.map(infer_pattern(c, n, elem))
          #(c, n, [pattern, ..patterns])
        }),
      )
      let elems = list.reverse(elems)

      // Create the tuple type from the inferred element types
      let typ = TupleType(list.map(elems, fn(e) { e.typ }))

      #(c, n, PatternTuple(typ, elems))
    }
    g.PatternList(_, elements, tail) -> {
      // Infer types for all elements in the list pattern
      use #(c, n, elements) <- result.try(
        list.try_fold(elements, #(c, n, []), fn(acc, elem) {
          let #(c, n, patterns) = acc
          use #(c, n, pattern) <- result.map(infer_pattern(c, n, elem))
          #(c, n, [pattern, ..patterns])
        }),
      )
      let elements = list.reverse(elements)

      // Create a type variable for the element type
      let #(c, elem_type) = new_type_var_ref(c)

      // Unify all element types with the element type variable
      use c <- result.try(
        list.try_fold(elements, c, fn(c, elem) { unify(c, elem.typ, elem_type) }),
      )

      // Create the list type
      let typ = NamedType("List", builtin, [elem_type])

      // Handle the tail pattern if present
      use #(c, n, tail) <- result.map(case tail {
        Some(tail_pattern) -> {
          use #(c, n, tail) <- result.try(infer_pattern(c, n, tail_pattern))
          // The tail should be a list of the same type
          use c <- result.map(unify(c, tail.typ, typ))
          #(c, n, Some(tail))
        }
        None -> Ok(#(c, n, None))
      })

      #(c, n, PatternList(typ, elements, tail))
    }
    g.PatternAssignment(_, pattern, name) -> {
      // First, infer the type of the inner pattern
      use #(c, n, pattern) <- result.map(infer_pattern(c, n, pattern))

      // Create the PatternAssignment with the same type as the inner pattern
      let pattern = PatternAssignment(pattern.typ, pattern, name)

      // Add the name binding to the environment
      let n = dict.insert(n, name, pattern.typ)

      #(c, n, pattern)
    }
    g.PatternConcatenate(_, prefix, prefix_name, rest_name) -> {
      // Add prefix_name to the environment if applicable
      let #(n, prefix_name) = case prefix_name {
        Some(g.Named(name)) -> {
          let n = dict.insert(n, name, string_type)
          #(n, Some(Named(name)))
        }
        Some(g.Discarded(name)) -> #(n, Some(Discarded(name)))
        None -> #(n, None)
      }

      // Add rest_name to the environment if applicable
      let #(n, rest_name_result) = case rest_name {
        g.Named(name) -> {
          let n = dict.insert(n, name, string_type)
          #(n, Named(name))
        }
        g.Discarded(name) -> #(n, Discarded(name))
      }

      let pattern =
        PatternConcatenate(string_type, prefix, prefix_name, rest_name_result)

      Ok(#(c, n, pattern))
    }
    g.PatternBitString(_, segments) -> {
      use #(c, n, segs) <- result.map(
        list.try_fold(segments, #(c, n, []), fn(acc, seg) {
          let #(c, n, segs) = acc
          let #(pattern, options) = seg

          use #(c, n, options, typ) <- result.try(
            list.try_fold(options, #(c, n, [], None), fn(acc, option) {
              let #(c, n, options, typ) = acc
              use #(c, n, option, option_type) <- result.try(case option {
                g.BigOption -> Ok(#(c, n, BigOption, None))
                g.LittleOption -> Ok(#(c, n, LittleOption, None))
                g.NativeOption -> Ok(#(c, n, NativeOption, None))
                g.SignedOption -> Ok(#(c, n, SignedOption, None))
                g.UnsignedOption -> Ok(#(c, n, UnsignedOption, None))
                g.BytesOption -> Ok(#(c, n, BytesOption, Some(bit_array_type)))
                g.BitsOption -> Ok(#(c, n, BitsOption, Some(bit_array_type)))
                g.IntOption -> Ok(#(c, n, IntOption, Some(int_type)))
                g.FloatOption -> Ok(#(c, n, FloatOption, Some(float_type)))
                g.Utf8Option -> Ok(#(c, n, Utf8Option, Some(string_type)))
                g.Utf16Option -> Ok(#(c, n, Utf16Option, Some(string_type)))
                g.Utf32Option -> Ok(#(c, n, Utf32Option, Some(string_type)))
                g.Utf8CodepointOption ->
                  Ok(#(c, n, Utf8CodepointOption, Some(codepoint_type)))
                g.Utf16CodepointOption ->
                  Ok(#(c, n, Utf16CodepointOption, Some(codepoint_type)))
                g.Utf32CodepointOption ->
                  Ok(#(c, n, Utf32CodepointOption, Some(codepoint_type)))
                g.SizeOption(size) -> Ok(#(c, n, SizeOption(size), None))
                g.SizeValueOption(pattern) -> {
                  use #(c, n, p) <- result.try(infer_pattern(c, n, pattern))
                  use c <- result.map(unify(c, p.typ, int_type))
                  #(c, n, SizeValueOption(p), None)
                }
                g.UnitOption(unit) -> Ok(#(c, n, UnitOption(unit), None))
              })
              use typ <- result.map(case typ, option_type {
                Some(_), Some(_) ->
                  Error(BitPatternSegmentTypeOverSpecified(location(c)))
                Some(_), None -> Ok(typ)
                _, _ -> Ok(option_type)
              })
              #(c, n, [option, ..options], typ)
            }),
          )
          let options = list.reverse(options)

          // If no type option was specified, default to int_type
          let expected_type = case typ {
            Some(t) -> t
            None -> int_type
          }

          use #(c, n, pattern) <- result.try(infer_pattern(c, n, pattern))
          use c <- result.map(unify(c, pattern.typ, expected_type))
          #(c, n, [#(pattern, options), ..segs])
        }),
      )
      let segs = list.reverse(segs)

      // The overall pattern type should be bit_array_type
      #(c, n, PatternBitString(bit_array_type, segs))
    }
    g.PatternVariant(span, module, constructor, arguments, with_spread) -> {
      // was a module name provided
      let with_module = case module {
        Some(_) -> True
        None -> False
      }

      // resolve the constructor function
      use #(module, constructor, poly, labels) <- result.try(
        resolve_constructor(c, module, constructor),
      )

      // infer the type of all arguments
      use #(c, n, arguments) <- result.try(
        list.try_fold(arguments, #(c, n, []), fn(acc, arg) {
          let #(c, n, arguments) = acc

          let #(item, label) = case arg {
            g.LabelledField(label, item) -> #(item, Some(label))
            g.ShorthandField(label) -> #(
              g.PatternVariable(span, label),
              Some(label),
            )
            g.UnlabelledField(item) -> #(item, None)
          }

          use #(c, n, arg2) <- result.map(infer_pattern(c, n, item))
          #(c, n, [Field(label, arg2), ..arguments])
        }),
      )
      let arguments = list.reverse(arguments)

      // handle labels
      use #(c, ordered_arguments) <- result.try(case with_spread {
        True -> {
          let #(c, args) =
            match_labels_optional(arguments, labels)
            |> list.fold(#(c, []), fn(acc, opt) {
              let #(c, opts) = acc
              let #(c, opt) = case opt {
                Some(opt) -> #(c, opt)
                None -> {
                  let #(c, typ) = new_type_var_ref(c)
                  #(c, Field(None, PatternDiscard(typ, "")))
                }
              }
              #(c, [opt, ..opts])
            })
          Ok(#(c, list.reverse(args)))
        }
        False -> {
          use args <- result.map(match_labels(c, arguments, labels))
          #(c, args)
        }
      })
      let arg_types = list.map(ordered_arguments, fn(x) { x.item.typ })

      // handle 0 parameter variants are not functions
      use #(c, typ) <- result.map(case arg_types {
        [] -> Ok(instantiate(c, poly))
        _ -> {
          // unify the constructor function type with the types of args
          let #(c, fun_typ) = instantiate(c, poly)
          let #(c, typ) = new_type_var_ref(c)
          use c <- result.map(unify(c, fun_typ, FunctionType(arg_types, typ)))
          #(c, typ)
        }
      })

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

fn resolve_constructor(c: Context, module: Option(String), constructor: String) {
  use constructor <- result.try(resolve_constructor_name(c, module, constructor))
  case constructor {
    FunctionGlobal(module:, name:, typ:, labels:) ->
      Ok(#(module, name, typ, labels))
    ConstantGlobal(..) ->
      Error(NotAFunction(
        location(c),
        constructor.module <> "." <> constructor.name,
      ))
  }
}

fn infer_annotation(
  c: Context,
  typ: g.Type,
) -> Result(#(Context, Annotation), Error) {
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
) -> Result(#(Context, List(Statement)), Error) {
  case body {
    [] -> Ok(#(c, []))
    [x, ..xs] ->
      case x {
        g.Expression(value) -> {
          use #(c, value) <- result.try(infer_expression(c, n, value))

          let statement = Expression(value.typ, value)

          // infer the rest of the body
          use #(c, rest) <- result.map(infer_body(c, n, xs))
          #(c, [statement, ..rest])
        }
        g.Assignment(_, kind, pattern, annotation, value) -> {
          // infer value before binding the new variable
          use #(c, value) <- result.try(infer_expression(c, n, value))

          // infer pattern, annotation, and value
          use #(c, n, pattern) <- result.try(infer_pattern(c, n, pattern))

          // if there is an annotation, the pattern must unify with the annotation
          use #(c, annotation) <- result.try(case annotation {
            Some(typ) -> {
              use #(c, annotation) <- result.try(infer_annotation(c, typ))
              use c <- result.map(unify(c, pattern.typ, annotation.typ))
              #(c, Some(annotation))
            }
            None -> Ok(#(c, None))
          })

          // the pattern must unify with both the annotation
          // and the assigned value
          use c <- result.try(unify(c, pattern.typ, value.typ))

          // TODO check the right "kind" was used (needs exhaustive checking)
          let kind = case kind {
            g.Let -> Let
            g.LetAssert(_message) -> LetAssert
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
          use #(c, rest) <- result.map(infer_body(c, n, xs))
          #(c, [statement, ..rest])
        }
        g.Assert(_, expression, message) -> {
          use #(c, expression) <- result.try(infer_expression(c, n, expression))

          use #(c, message) <- result.try(case message {
            Some(msg) -> {
              use #(c, msg) <- result.map(infer_expression(c, n, msg))
              #(c, Some(msg))
            }
            None -> Ok(#(c, None))
          })

          let statement = Assert(expression.typ, expression, message)

          // infer the rest of the body
          use #(c, rest) <- result.map(infer_body(c, n, xs))
          #(c, [statement, ..rest])
        }
        g.Use(span, patterns, function) -> {
          // TODO infer without desugaring
          let #(span, fun, args) = case function {
            g.Call(span, fun, args) -> #(span, fun, args)
            _ -> #(span, function, [])
          }
          let params =
            list.index_map(patterns, fn(_pat, i) {
              g.FnParameter(g.Named("P" <> int.to_string(i)), None)
            })
          let body =
            list.index_fold(patterns, xs, fn(body, pat, i) {
              let param = g.Variable(span, "P" <> int.to_string(i))
              let assignment =
                g.Assignment(span, g.Let, pat.pattern, None, param)
              [assignment, ..body]
            })
          let callback = g.Fn(span, params, None, body)
          use #(_, ifun) <- result.try(infer_expression(c, n, fun))
          let field = case ifun {
            Function(labels:, ..) ->
              case list.last(labels) {
                Ok(Some(label)) -> g.LabelledField(label, callback)
                _ -> g.UnlabelledField(callback)
              }
            _ -> g.UnlabelledField(callback)
          }
          let call = g.Call(span, fun, list.append(args, [field]))
          use #(c, exp) <- result.map(infer_expression(c, n, call))
          let statement = Expression(exp.typ, exp)
          #(c, [statement])
        }
      }
  }
}

fn match_labels(
  c: Context,
  args: List(Field(a)),
  params: List(Option(String)),
) -> Result(List(Field(a)), Error) {
  do_match_labels(c, args, params, #(list.length(params), list.length(args)))
}

fn do_match_labels(
  c: Context,
  args: List(Field(a)),
  params: List(Option(String)),
  lens: #(Int, Int),
) -> Result(List(Field(a)), Error) {
  // find the labels in the order specified by parameters
  // either we find the matching label or default to the first unlabelled arg
  case params {
    [] ->
      case args {
        [] -> Ok([])
        _ -> Error(WrongArity(location(c), lens.0, lens.1))
      }
    [p, ..p_rest] ->
      listx.pop(args, fn(a) { a.label == p })
      |> result.try_recover(fn(_) { listx.pop(args, fn(a) { a.label == None }) })
      |> result.map_error(fn(_) {
        case p {
          Some(l) -> LabelNotFound(location(c), l)
          None -> WrongArity(location(c), lens.0, lens.1)
        }
      })
      |> result.try(fn(r) {
        let #(a, a_rest) = r
        use rest <- result.map(match_labels(c, a_rest, p_rest))
        [a, ..rest]
      })
  }
}

fn match_labels_optional(
  args: List(Field(a)),
  params: List(Option(String)),
) -> List(Option(Field(a))) {
  // find the labels in the order specified by parameters
  case params {
    [] -> []
    [p, ..p_rest] ->
      case listx.pop(args, fn(a) { a.label == p }) {
        Ok(#(a, a_rest)) -> [Some(a), ..match_labels_optional(a_rest, p_rest)]
        Error(_) -> [None, ..match_labels_optional(args, p_rest)]
      }
  }
}

fn infer_expression(
  c: Context,
  n: LocalEnv,
  exp: g.Expression,
) -> Result(#(Context, Expression), Error) {
  let c = Context(..c, current_span: exp.location)
  case exp {
    g.Int(_, s) -> Ok(#(c, Int(int_type, s)))
    g.Float(_, s) -> Ok(#(c, Float(float_type, s)))
    g.String(_, s) -> Ok(#(c, String(string_type, s)))
    g.Variable(_, s) -> {
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
    g.NegateInt(_, value) -> {
      use #(c, e) <- result.try(infer_expression(c, n, value))
      use c <- result.map(unify(c, e.typ, int_type))
      #(c, NegateInt(int_type, e))
    }
    g.NegateBool(_, value) -> {
      use #(c, e) <- result.try(infer_expression(c, n, value))
      use c <- result.map(unify(c, e.typ, bool_type))
      #(c, NegateBool(bool_type, e))
    }
    g.Block(_, statements) -> {
      use #(c, statements) <- result.try(infer_body(c, n, statements))
      case list.last(statements) {
        Ok(last) -> Ok(#(c, Block(last.typ, statements)))
        Error(_) -> Error(EmptyBlock(location(c)))
      }
    }
    g.Panic(_, e) -> {
      case e {
        Some(e) -> {
          // the expression should be a string
          use #(c, e) <- result.try(infer_expression(c, n, e))
          use c <- result.map(unify(c, e.typ, string_type))
          let #(c, typ) = new_type_var_ref(c)
          #(c, Panic(typ, Some(e)))
        }
        None -> {
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Panic(typ, None)))
        }
      }
    }
    g.Todo(_, e) -> {
      case e {
        Some(e) -> {
          // the expression should be a string
          use #(c, e) <- result.try(infer_expression(c, n, e))
          use c <- result.map(unify(c, e.typ, string_type))
          let #(c, typ) = new_type_var_ref(c)
          #(c, Todo(typ, Some(e)))
        }
        None -> {
          let #(c, typ) = new_type_var_ref(c)
          Ok(#(c, Todo(typ, None)))
        }
      }
    }
    g.Tuple(_, elements) -> {
      // Infer type of all elements
      use #(c, elements) <- result.try(
        list.try_fold(elements, #(c, []), fn(acc, e) {
          let #(c, elements) = acc
          use #(c, e) <- result.try(infer_expression(c, n, e))
          Ok(#(c, [e, ..elements]))
        }),
      )
      let elements = list.reverse(elements)

      // Create tuple type
      let types = list.map(elements, fn(e) { e.typ })
      let typ = TupleType(types)
      Ok(#(c, Tuple(typ, elements)))
    }
    g.List(_, elements, rest) -> {
      // Infer types for all elements
      use #(c, elements) <- result.try(
        list.try_fold(elements, #(c, []), fn(acc, e) {
          let #(c, elements) = acc
          use #(c, e) <- result.try(infer_expression(c, n, e))
          Ok(#(c, [e, ..elements]))
        }),
      )
      let elements = list.reverse(elements)

      // Infer type for rest (if present)
      use #(c, rest) <- result.try(case rest {
        Some(t) -> {
          use #(c, t) <- result.try(infer_expression(c, n, t))
          Ok(#(c, Some(t)))
        }
        None -> Ok(#(c, None))
      })

      // Create a type variable for the element type
      let #(c, elem_type) = new_type_var_ref(c)
      let typ = NamedType("List", builtin, [elem_type])

      // Unify all element types
      use c <- result.try(
        list.try_fold(elements, c, fn(c, e) { unify(c, e.typ, elem_type) }),
      )

      // Unify rest type with list type (if rest is present)
      use c <- result.map(case rest {
        Some(t) -> unify(c, t.typ, typ)
        None -> Ok(c)
      })

      #(c, List(typ, elements, rest))
    }
    g.Fn(_, parameters, return_annotation, body) -> {
      infer_fn(c, n, parameters, return_annotation, body, None)
    }
    g.RecordUpdate(span, module, constructor, record, fields) -> {
      // Infer the type of the base record expression
      use #(c, base_expr) <- result.try(infer_expression(c, n, record))

      // Resolve the constructor type
      use #(res_module, constructor, poly, labels) <- result.try(
        resolve_constructor(c, module, constructor),
      )

      // Instantiate the constructor type
      let #(c, constructor_type) = instantiate(c, poly)
      // TODO: wrap in a result, or is this guaranteed to succeed?
      let assert FunctionType(constructor_args, constructor_ret) =
        constructor_type

      // Unify the base expression type with the constructor type
      use c <- result.try(unify(c, base_expr.typ, constructor_ret))

      // Infer types for all updated fields
      use #(c, updated_fields) <- result.try(
        list.try_fold(fields, #(c, []), fn(acc, field) {
          let #(c, updated_fields) = acc
          let item = case field.item {
            Some(item) -> item
            None -> g.Variable(span, field.label)
          }
          use #(c, value) <- result.map(infer_expression(c, n, item))
          #(c, [#(field.label, value), ..updated_fields])
        }),
      )
      let updated_fields = list.reverse(updated_fields)

      let fields = list.map(updated_fields, fn(x) { Field(Some(x.0), x.1) })
      let ordered_fields = match_labels_optional(fields, labels)
      use ordered_fields <- result.try(
        list.strict_zip(ordered_fields, constructor_args)
        |> result.map_error(fn(_) {
          WrongArity(
            location(c),
            list.length(constructor_args),
            list.length(ordered_fields),
          )
        }),
      )

      use #(c, ordered_fields) <- result.map(
        list.try_fold(ordered_fields, #(c, []), fn(acc, x) {
          let #(c, fields) = acc
          let #(given, expected) = x
          use #(c, result) <- result.map(case given {
            Some(e) -> {
              use c <- result.map(unify(c, e.item.typ, expected))
              #(c, Ok(e))
            }
            None -> Ok(#(c, Error(expected)))
          })
          #(c, [result, ..fields])
        }),
      )
      let ordered_fields = list.reverse(ordered_fields)

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

      #(c, record_update)
    }
    g.FieldAccess(_, container, label) -> {
      let field_access = {
        // try to infer the value, otherwise it might be a module access
        use #(c, value) <- result.try(infer_expression(c, n, container))

        // field access must be on a named type
        let value_typ = case resolve_type(c, value.typ) {
          NamedType(type_name, module, _) -> Ok(#(type_name, module))
          _ -> Error(InvalidFieldAccess(location(c)))
        }
        use #(type_name, module) <- result.try(value_typ)

        // find the custom type definition
        use custom <- result.try(resolve_custom_type(c, module, type_name))

        // access only works with one variant
        let variant = case custom.definition.variants {
          // TODO proper implementation checking all variants
          [variant, ..] -> Ok(variant)
          _ -> Error(InvalidFieldAccess(location(c)))
        }
        use variant <- result.try(variant)

        // find the matching field and index
        let field =
          variant.fields
          |> list.index_map(fn(x, i) { #(x, i) })
          |> list.find(fn(x) { { x.0 }.label == Some(label) })
          |> result.replace_error(FieldNotFound(location(c), label))
        use #(field, index) <- result.try(field)

        // create a getter function type
        let getter = FunctionType([custom.definition.typ.typ], field.item.typ)
        let getter = Poly(custom.definition.typ.vars, getter)
        let #(c, getter) = instantiate(c, getter)

        // unify the getter as if we're calling it on the value
        let #(c, typ) = new_type_var_ref(c)
        use c <- result.map(unify(c, getter, FunctionType([value.typ], typ)))

        #(c, FieldAccess(typ, value, module, variant.name, label, index))
      }
      case field_access {
        Ok(access) -> Ok(access)
        Error(e) -> {
          // try a module access instead
          case container {
            g.Variable(_, module) -> {
              case resolve_aliased_global(c, QualifiedName(module, label)) {
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
    g.Call(span, function, arguments) -> {
      // infer the type of the function
      use #(c, fun) <- result.try(infer_expression(c, n, function))

      // handle labels
      let labels = case fun {
        Function(labels:, ..) -> labels
        _ -> list.map(arguments, fn(_) { None })
      }

      let args =
        list.map(arguments, fn(arg) {
          let #(label, arg) = case arg {
            g.LabelledField(label, item) -> #(Some(label), item)
            g.ShorthandField(label) -> #(Some(label), g.Variable(span, label))
            g.UnlabelledField(item) -> #(None, item)
          }
          Field(label, arg)
        })

      use args <- result.try(match_labels(c, args, labels))

      // use fun parameter type as type hints for inferring arguments
      use args <- result.try(case resolve_type(c, fun.typ) {
        FunctionType(params, _ret) -> {
          let params = list.map(params, Some)
          use args <- result.map(
            list.strict_zip(params, args)
            |> result.map_error(fn(_) {
              WrongArity(location(c), list.length(params), list.length(args))
            }),
          )
          args
        }
        _ -> Ok(list.map(args, fn(arg) { #(None, arg) }))
      })
      // infer the type of all args
      use #(c, args) <- result.try(
        list.try_fold(args, #(c, []), fn(acc, type_hint_and_field) {
          let #(c, args) = acc
          let #(type_hint, field_arg) = type_hint_and_field

          // give type hint when arg is a fn
          let result = case field_arg.item {
            g.Fn(_, parameters, return_annotation, body) ->
              infer_fn(c, n, parameters, return_annotation, body, type_hint)
            _ -> infer_expression(c, n, field_arg.item)
          }
          use #(c, inferred_arg) <- result.try(result)

          use c <- result.map(case type_hint {
            Some(hint) -> unify(c, hint, inferred_arg.typ)
            None -> Ok(c)
          })

          let field_with_inferred = Field(field_arg.label, inferred_arg)

          #(c, [field_with_inferred, ..args])
        }),
      )
      let args = list.reverse(args)
      let arg_types = list.map(args, fn(field) { field.item.typ })
      let ordered_arguments = list.map(args, fn(field) { field.item })
      // unify the function type with the types of args
      let #(c, typ) = new_type_var_ref(c)
      use c <- result.map(unify(c, fun.typ, FunctionType(arg_types, typ)))
      #(c, Call(typ, fun, ordered_arguments))
    }
    g.TupleIndex(_, tuple, index) -> {
      use #(c, tuple) <- result.try(infer_expression(c, n, tuple))
      case resolve_type(c, tuple.typ) {
        TupleType(elements) -> {
          tuple_index_type(c, elements, index)
          |> result.map(fn(typ) { #(c, TupleIndex(typ, tuple, index)) })
        }
        _ -> Error(InvalidTupleAccess(location(c)))
      }
    }
    g.FnCapture(span, label, function, arguments_before, arguments_after) -> {
      // TODO return non-desugared version
      let #(c, x) = new_temp_var(c)
      let arg = case label {
        Some(label) -> g.LabelledField(label, g.Variable(span, x))
        None -> g.UnlabelledField(g.Variable(span, x))
      }
      let args = list.flatten([arguments_before, [arg], arguments_after])
      let param = g.FnParameter(g.Named(x), None)
      let lambda =
        g.Fn(span, [param], None, [g.Expression(g.Call(span, function, args))])
      infer_expression(c, n, lambda)
    }
    g.BitString(_, segments) -> {
      use #(c, segs) <- result.try(
        list.try_fold(segments, #(c, []), fn(acc, seg) {
          let #(c, segs) = acc
          let #(expression, options) = seg
          use #(c, options, typ) <- result.try(
            list.try_fold(options, #(c, [], None), fn(acc, option) {
              let #(c, options, typ) = acc
              // TODO handle all options
              use #(c, option, option_type) <- result.try(case option {
                g.BigOption -> Ok(#(c, BigOption, None))
                g.BytesOption -> Ok(#(c, BytesOption, Some(bit_array_type)))
                g.BitsOption -> Ok(#(c, BitsOption, Some(bit_array_type)))
                g.FloatOption -> Ok(#(c, FloatOption, Some(float_type)))
                g.IntOption -> Ok(#(c, IntOption, Some(int_type)))
                g.LittleOption -> Ok(#(c, LittleOption, None))
                g.NativeOption -> Ok(#(c, NativeOption, None))
                g.SignedOption -> Ok(#(c, SignedOption, None))
                g.SizeOption(size) -> Ok(#(c, SizeOption(size), None))
                g.SizeValueOption(e) -> {
                  use #(c, e) <- result.try(infer_expression(c, n, e))
                  use c <- result.map(unify(c, e.typ, int_type))
                  #(c, SizeValueOption(e), None)
                }
                g.UnitOption(unit) -> Ok(#(c, UnitOption(unit), None))
                g.UnsignedOption -> Ok(#(c, UnsignedOption, None))
                g.Utf16CodepointOption ->
                  Ok(#(c, Utf16CodepointOption, Some(codepoint_type)))
                g.Utf16Option -> Ok(#(c, Utf16Option, Some(string_type)))
                g.Utf32CodepointOption ->
                  Ok(#(c, Utf32CodepointOption, Some(codepoint_type)))
                g.Utf32Option -> Ok(#(c, Utf32Option, Some(string_type)))
                g.Utf8CodepointOption ->
                  Ok(#(c, Utf8CodepointOption, Some(codepoint_type)))
                g.Utf8Option -> Ok({ #(c, Utf8Option, Some(string_type)) })
              })
              use typ <- result.map(case typ, option_type {
                Some(_), Some(_) ->
                  Error(BitPatternSegmentTypeOverSpecified(location(c)))
                Some(_), None -> Ok(typ)
                _, _ -> Ok(option_type)
              })
              #(c, [option, ..options], typ)
            }),
          )
          let options = list.reverse(options)
          let typ = case typ {
            Some(typ) -> typ
            None -> int_type
          }
          use #(c, expression) <- result.try(infer_expression(c, n, expression))
          use c <- result.map(unify(c, expression.typ, typ))
          #(c, [#(expression, options), ..segs])
        }),
      )
      let segs = list.reverse(segs)
      Ok(#(c, BitString(bit_array_type, segs)))
    }
    g.Case(_, subjects, clauses) -> {
      use #(c, subjects) <- result.try(
        list.try_fold(subjects, #(c, []), fn(acc, sub) {
          let #(c, subjects) = acc
          use #(c, sub) <- result.try(infer_expression(c, n, sub))
          Ok(#(c, [sub, ..subjects]))
        }),
      )
      let subjects = list.reverse(subjects)

      // all of the branches should unify with the case type
      let #(c, typ) = new_type_var_ref(c)

      use #(c, clauses) <- result.try(
        list.try_fold(clauses, #(c, []), fn(acc, clause) {
          let #(c, clauses) = acc

          // patterns is a List(List(Pattern))
          // the inner list has a pattern to match each subject
          // the outer list has alternatives that have the same body
          use #(c, n, patterns) <- result.try(
            list.try_fold(clause.patterns, #(c, n, []), fn(acc, pat) {
              let #(c, n, pats) = acc

              // each pattern has a corresponding subject
              use sub_pats <- result.try(
                list.strict_zip(subjects, pat)
                |> result.map_error(fn(_) {
                  WrongArity(
                    location(c),
                    list.length(subjects),
                    list.length(pat),
                  )
                }),
              )
              use #(c, n, pat) <- result.map(
                list.try_fold(sub_pats, #(c, n, []), fn(acc, sub_pat) {
                  let #(c, n, pats) = acc
                  let #(sub, pat) = sub_pat
                  use #(c, n, pat) <- result.try(infer_pattern(c, n, pat))
                  // the pattern type should match the corresponding subject
                  use c <- result.map(unify(c, pat.typ, sub.typ))
                  #(c, n, [pat, ..pats])
                }),
              )
              let pat = list.reverse(pat)

              // all alternatives must bind the same names
              // TODO check the alternative patterns bind the same names
              // how do we check this? do we need to unify (based on name)?
              // maybe infer_pattern needs to return a list of bindings
              // instead of a new env

              #(c, n, [pat, ..pats])
            }),
          )
          let patterns = list.reverse(patterns)

          // if the guard exists ensure it has a boolean result
          use #(c, guard) <- result.try(case clause.guard {
            Some(guard) -> {
              use #(c, guard) <- result.try(infer_expression(c, n, guard))
              use c <- result.map(unify(c, guard.typ, bool_type))
              #(c, Some(guard))
            }
            None -> Ok(#(c, None))
          })

          use #(c, body) <- result.try(infer_expression(c, n, clause.body))

          // the body should unify with the case type
          use c <- result.map(unify(c, typ, body.typ))

          let santa = Clause(patterns:, guard:, body:)
          #(c, [santa, ..clauses])
        }),
      )
      let clauses = list.reverse(clauses)

      Ok(#(c, Case(typ:, subjects:, clauses:)))
    }
    g.BinaryOperator(span, g.Pipe, left, right) -> {
      // TODO return a not-desugared version
      case right {
        g.Call(span, fun, args) -> {
          let call = g.Call(span, fun, [g.UnlabelledField(left), ..args])
          infer_expression(c, n, call)
        }
        g.FnCapture(span, label, fun, before, after) -> {
          let args = case label {
            Some(label) -> [before, [g.LabelledField(label, left)], after]
            None -> [before, [g.UnlabelledField(left)], after]
          }
          infer_expression(c, n, g.Call(span, fun, list.flatten(args)))
        }
        g.Echo(span, None) -> {
          let echo_ = g.Variable(span, "echo_")
          let pipe = g.BinaryOperator(span, g.Pipe, left, echo_)
          infer_expression(c, n, pipe)
        }
        _ -> {
          let call = g.Call(span, right, [g.UnlabelledField(left)])
          infer_expression(c, n, call)
        }
      }
    }
    g.BinaryOperator(_, name, left, right) -> {
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

      use #(c, left) <- result.try(infer_expression(c, n, left))
      use #(c, right) <- result.try(infer_expression(c, n, right))

      // unify the function type with the types of args
      let #(c, typ) = new_type_var_ref(c)
      use c <- result.map(unify(
        c,
        fun_typ,
        FunctionType([left.typ, right.typ], typ),
      ))

      #(c, BinaryOperator(typ, name, left, right))
    }
    g.Echo(_, expression) -> {
      case expression {
        Some(expr) -> {
          use #(c, expr) <- result.try(infer_expression(c, n, expr))
          Ok(#(c, Echo(expr.typ, Some(expr))))
        }
        None -> {
          Ok(#(c, Echo(nil_type, None)))
        }
      }
    }
  }
}

fn resolve_custom_type(c: Context, module: String, type_name: String) {
  use mod <- result.try(get_module(c, module))
  list.find(mod.custom_types, fn(x) { x.definition.name == type_name })
  |> result.replace_error(UnresolvedType(location(c), type_name))
}

fn tuple_index_type(
  c: Context,
  elements: List(Type),
  index: Int,
) -> Result(Type, Error) {
  index_into_list(elements, index)
  |> result.map_error(fn(_) {
    TupleIndexOutOfBounds(location(c), list.length(elements), index)
  })
}

fn index_into_list(list: List(a), index: Int) -> Result(a, Nil) {
  case index, list {
    0, [item, ..] -> Ok(item)
    _, [_, ..rest] -> index_into_list(rest, index - 1)
    _, _ -> Error(Nil)
  }
}

fn infer_fn(
  c: Context,
  n: Dict(String, Type),
  parameters: List(g.FnParameter),
  return: Option(g.Type),
  body: List(g.Statement),
  hint: Option(Type),
) -> Result(#(Context, Expression), Error) {
  // map parameters to FunctionParameter for code reuse
  let parameters =
    list.map(parameters, fn(p) { g.FunctionParameter(None, p.name, p.type_) })

  use #(c, parameters, return) <- result.try(infer_function_parameters(
    c,
    parameters,
    return,
  ))

  let #(c, return_type) = case return {
    Some(x) -> #(c, x.typ)
    None -> new_type_var_ref(c)
  }

  // compute function type
  let parameter_types = list.map(parameters, fn(x) { x.typ })
  let typ = FunctionType(parameter_types, return_type)

  // unify parameters with type hint
  use c <- result.try(case hint {
    Some(hint) -> unify(c, typ, hint)
    None -> Ok(c)
  })

  // put params into local env
  let n =
    list.fold(parameters, n, fn(n, param) {
      case param.name {
        Named(name) -> dict.insert(n, name, param.typ)
        Discarded(_) -> n
      }
    })

  // infer body
  use #(c, body) <- result.try(infer_body(c, n, body))

  // unify the return type with the last statement
  use c <- result.map(case list.last(body) {
    Ok(statement) -> unify(c, return_type, statement.typ)
    Error(_) -> Ok(c)
  })

  let fun = Fn(typ:, parameters:, return:, body:)
  #(c, fun)
}

type PolyEnv =
  Dict(Int, Type)

fn get_type_var(c: Context, var: Ref) {
  // TODO: guaranteed to succeed?
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

fn unify(c: Context, a: Type, b: Type) -> Result(Context, Error) {
  let a = resolve_type(c, a)
  let b = resolve_type(c, b)
  case a, b {
    VariableType(ref), b ->
      case a == b {
        True -> Ok(c)
        False -> {
          // TODO: guaranteed to succeed?
          let assert Unbound(aid) = get_type_var(c, ref)
          let #(c, occurs) = occurs(c, aid, b)
          case occurs {
            True -> Error(RecursiveTypeError(location(c)))
            False -> Ok(set_type_var(c, ref, Bound(b)))
          }
        }
      }
    a, VariableType(_) -> unify(c, b, a)
    NamedType(aname, amodule, _), NamedType(bname, bmodule, _)
      if aname != bname || amodule != bmodule
    -> Error(IncompatibleTypes(location(c), a, b))
    NamedType(_, _, aargs), NamedType(_, _, bargs) ->
      unify_arguments(c, aargs, bargs)
    FunctionType(aargs, aret), FunctionType(bargs, bret) -> {
      use c <- result.try(unify(c, aret, bret))
      unify_arguments(c, aargs, bargs)
    }
    TupleType(aelements), TupleType(belements) -> {
      unify_arguments(c, aelements, belements)
    }
    _, _ -> Error(IncompatibleTypes(location(c), a, b))
  }
}

fn unify_arguments(
  c: Context,
  aargs: List(Type),
  bargs: List(Type),
) -> Result(Context, Error) {
  use args <- result.try(
    list.strict_zip(aargs, bargs)
    |> result.map_error(fn(_) {
      WrongArity(location(c), list.length(aargs), list.length(bargs))
    }),
  )
  list.try_fold(args, c, fn(c, x) { unify(c, x.0, x.1) })
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
pub fn resolve_type(c: Context, typ: Type) -> Type {
  case typ {
    VariableType(x) -> {
      // TODO: guaranteed to succeed?
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
      // TODO: guaranteed to succeed?
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

fn location(c: Context) {
  Location(c.current_module, c.current_definition, c.current_span)
}
