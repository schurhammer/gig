import glance_typed as t
import gleam/int
import gleam/string

pub fn field_name(label: String, index: Int) {
  case label {
    "" -> "field" <> int.to_string(index)
    _ -> label
  }
}

pub fn get_tuple_id(size: Int) {
  "Tuple" <> int.to_string(size)
}

pub fn get_id(module: String, name: String) -> String {
  case module == t.prelude {
    True -> name
    False -> string.replace(module, "/", "_") <> "_" <> name
  }
}
