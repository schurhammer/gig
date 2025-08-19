import glance
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

pub fn apply(module: glance.Module, patch: glance.Module) -> glance.Module {
  // patch imports
  let imports =
    module.imports
    |> list.map(fn(i) { #(i.definition.module, i) })
  let patch_imports =
    patch.imports
    |> list.map(fn(i) { #(i.definition.module, i) })
  let merged_imports =
    merge(imports, patch_imports, fn(a, b) {
      glance.Definition(
        b.attributes,
        glance.Import(
          location: b.definition.location,
          module: b.definition.module,
          alias: b.definition.alias,
          unqualified_types: list.append(
            a.definition.unqualified_types,
            b.definition.unqualified_types,
          ),
          unqualified_values: list.append(
            a.definition.unqualified_values,
            b.definition.unqualified_values,
          ),
        ),
      )
    })

  // patch functions
  let functions =
    module.functions
    |> list.map(fn(f) { #(f.definition.name, f) })
  let patch_functions =
    patch.functions
    |> list.map(fn(f) { #(f.definition.name, f) })
  let merged_functions =
    merge(functions, patch_functions, fn(a, b) {
      check_matching_args(a.definition, b.definition)
      b
    })

  // patch custom types
  let custom_types =
    list.map(module.custom_types, fn(t) { #(t.definition.name, t) })
  let patch_custom_types =
    list.map(patch.custom_types, fn(t) { #(t.definition.name, t) })
  let merged_custom_types =
    merge(custom_types, patch_custom_types, fn(_, b) { b })

  // patch constants
  let constants = list.map(module.constants, fn(c) { #(c.definition.name, c) })
  let patch_constants =
    list.map(patch.constants, fn(c) { #(c.definition.name, c) })
  let merged_constants = merge(constants, patch_constants, fn(_, b) { b })

  // patch type aliases
  let type_aliases =
    list.map(module.type_aliases, fn(a) { #(a.definition.name, a) })
  let patch_type_aliases =
    list.map(patch.type_aliases, fn(a) { #(a.definition.name, a) })
  let merged_type_aliases =
    merge(type_aliases, patch_type_aliases, fn(_, b) { b })

  glance.Module(
    imports: merged_imports,
    functions: merged_functions,
    custom_types: merged_custom_types,
    constants: merged_constants,
    type_aliases: merged_type_aliases,
  )
}

fn check_matching_args(af: glance.Function, bf: glance.Function) {
  let args = case list.strict_zip(af.parameters, bf.parameters) {
    Ok(args) -> args
    Error(_) -> {
      panic as { "patch should have matching parameters. \n at " <> bf.name }
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
          "patch should have matching labels. \n at "
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

fn merge(
  a: List(#(String, glance.Definition(d))),
  b: List(#(String, glance.Definition(d))),
  merge_item: fn(glance.Definition(d), glance.Definition(d)) ->
    glance.Definition(d),
) -> List(glance.Definition(d)) {
  // filter out other targets
  let a =
    list.filter(a, fn(a) {
      !list.any({ a.1 }.attributes, fn(attr) {
        case attr {
          glance.Attribute("target", [glance.Variable(_, c)]) if c != "c" ->
            True
          _ -> False
        }
      })
    })

  // merge a's that are in b using the merge function
  let bd = dict.from_list(b)
  let a =
    list.map(a, fn(a) {
      case dict.get(bd, a.0) {
        Ok(b) -> #(a.0, merge_item(a.1, b))
        _ -> a
      }
    })

  // keep b's that are not in a
  let ad = dict.from_list(a)
  let b = list.filter(b, fn(b) { !dict.has_key(ad, b.0) })

  list.append(a, b)
  |> list.map(fn(a) { a.1 })
}
