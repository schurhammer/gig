import gig/typed_ast as t
import gleam/int
import gleam/list
import gleam/string

pub fn get_field_name(index: Int) {
  "f" <> int.to_string(index)
}

pub fn get_getter_name(variant: String, index: Int) {
  variant <> "_" <> get_field_name(index)
}

pub fn get_constructor_name(variant: String) {
  variant <> "_NEW"
}

pub fn get_id(module: String, name: String) -> String {
  case module == t.builtin {
    True -> name
    False -> string.replace(module, "/", "_") <> "_" <> name
  }
}
