import gig/typed_ast as t
import gleam/int
import gleam/string

pub fn get_field_name(label: String, index: Int) {
  case label {
    "" -> "field" <> int.to_string(index)
    _ -> label
  }
}

pub fn get_tuple_id(size: Int) {
  "Tuple" <> int.to_string(size)
}

pub fn get_id(module: String, name: String) -> String {
  case module == t.builtin {
    True -> name
    False -> string.replace(module, "/", "_") <> "_" <> name
  }
}
