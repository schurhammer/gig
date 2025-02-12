import glance
import gleam/dict
import gleam/list

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

  let polyfill_functions =
    polyfill.functions
    |> list.map(fn(f) { #(f.definition.name, f) })
    |> dict.from_list()

  let merged_functions =
    dict.merge(functions, polyfill_functions)
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
