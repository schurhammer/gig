import glance
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

pub fn apply(module: glance.Module, polyfill: glance.Module) -> glance.Module {
  // polyfill imports
  let imports =
    module.imports
    |> list.map(fn(i) { #(i.definition.module, i) })
    |> dict.from_list()

  let merged_imports =
    list.fold(polyfill.imports, imports, fn(acc, i) {
      let i = case dict.get(acc, i.definition.module) {
        Ok(existing) -> {
          glance.Definition(
            i.attributes,
            glance.Import(
              module: i.definition.module,
              alias: i.definition.alias,
              unqualified_types: list.append(
                existing.definition.unqualified_types,
                i.definition.unqualified_types,
              ),
              unqualified_values: list.append(
                existing.definition.unqualified_values,
                i.definition.unqualified_values,
              ),
            ),
          )
        }
        Error(_) -> i
      }
      dict.insert(acc, i.definition.module, i)
    })
    |> dict.values()

  // polyfill functions
  let functions =
    module.functions
    |> list.map(fn(f) { #(f.definition.name, f) })
    |> dict.from_list()

  let merged_functions =
    list.fold(polyfill.functions, functions, fn(funs, i) {
      case dict.get(funs, i.definition.name) {
        Ok(existing) -> check_matching_args(existing.definition, i.definition)
        Error(_) -> Nil
      }
      dict.insert(funs, i.definition.name, i)
    })
    |> dict.values()

  // polyfill custom types
  let custom_types =
    module.custom_types
    |> list.map(fn(t) { #(t.definition.name, t) })
    |> dict.from_list()

  let polyfill_custom_types =
    polyfill.custom_types
    |> list.map(fn(t) { #(t.definition.name, t) })
    |> dict.from_list()

  let merged_custom_types =
    dict.merge(custom_types, polyfill_custom_types)
    |> dict.values()

  // polyfill constants
  let constants =
    module.constants
    |> list.map(fn(c) { #(c.definition.name, c) })
    |> dict.from_list()

  let polyfill_constants =
    polyfill.constants
    |> list.map(fn(c) { #(c.definition.name, c) })
    |> dict.from_list()

  let merged_constants =
    dict.merge(constants, polyfill_constants)
    |> dict.values()

  // polyfill type aliases
  let type_aliases =
    module.type_aliases
    |> list.map(fn(a) { #(a.definition.name, a) })
    |> dict.from_list()

  let polyfill_type_aliases =
    polyfill.type_aliases
    |> list.map(fn(a) { #(a.definition.name, a) })
    |> dict.from_list()

  let merged_type_aliases =
    dict.merge(type_aliases, polyfill_type_aliases)
    |> dict.values()

  glance.Module(
    functions: merged_functions,
    custom_types: merged_custom_types,
    constants: merged_constants,
    type_aliases: merged_type_aliases,
    imports: merged_imports,
  )
}

fn check_matching_args(af: glance.Function, bf: glance.Function) {
  let args = case list.strict_zip(af.parameters, bf.parameters) {
    Ok(args) -> args
    Error(_) -> {
      panic as { "polyfill should have matching parameters. \n at " <> bf.name }
    }
  }
  list.each(args, fn(arg) {
    let #(a, b) = arg
    let a_label = case a.label {
      Some(label) -> label
      None -> "None"
    }
    let b_label = case b.label {
      Some(label) -> label
      None -> "None"
    }
    case a.label == b.label {
      False -> {
        panic as {
          "polyfill should have matching labels. \n at "
          <> bf.name
          <> " "
          <> a_label
          <> "!="
          <> b_label
        }
      }
      True -> Nil
    }
    Nil
  })
  Nil
}
