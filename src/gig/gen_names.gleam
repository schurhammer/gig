import gig/typed_ast as t
import gleam/int
import gleam/string

pub fn get_field_name(index: Int) {
  "f" <> int.to_string(index)
}

pub fn get_tuple_id(size: Int) {
  "Tuple" <> int.to_string(size)
}

pub fn get_getter_name(variant: String, index: Int) {
  get_field_name(index) <> "_" <> variant
}

pub fn get_variant_check_name(variant: String) {
  "isa_" <> variant
}

pub fn get_id(module: String, name: String) -> String {
  case module == t.builtin {
    True -> name
    False -> string.replace(module, "/", "_") <> "_" <> name
  }
}
