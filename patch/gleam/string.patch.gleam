// TODO figure out the minimum required external functions to be able to
// implement all the string functionality
// maybe we can build on bitstring / dynamic types once its supported
import gleam
import gleam/order
import gleam/list
import gleam/string_tree

pub fn length(a: String) -> Int {
  length_string(a)
}

pub fn byte_size(a: String) -> Int {
  length_string(a)
}

@external(c, "", "append_string")
pub fn append(to first: String, suffix second: String) -> String

@external(c, "", "starts_with_string")
pub fn starts_with(string: String, prefix: String) -> Bool

@external(c, "", "ends_with_string")
pub fn ends_with(string: String, suffix: String) -> Bool

@external(c, "", "compare_string")
fn do_compare(a: String, b: String) -> Int

@external(c, "", "slice_string")
fn do_slice(string: String, idx: Int, len: Int) -> String

pub fn compare(a: String, b: String) -> order.Order {
    case do_compare(a, b) {
        x if x < 0 -> order.Lt
        x if x > 0 -> order.Gt
        _ -> order.Eq
    }
}

@external(c, "", "pop_grapheme_string")
pub fn pop_grapheme(string: String) -> Result(#(String, String), Nil)

pub fn inspect(value: a) -> String {
  gleam.inspect(value)
}

pub fn join(strings: List(String), with separator: String) -> String {
  do_join(strings, separator)
}

fn do_join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(with: separator)
  |> concat
}

pub fn concat(strings: List(String)) -> String {
  case strings {
    [] -> ""
    [x] -> x
    xs -> concat(append_pairs(xs))
  }
}

fn append_pairs(strings: List(String)) {
  case strings {
    [] -> []
    [x] -> [x]
    [x, y, ..xs] -> [x <> y, ..append_pairs(xs)]
  }
}

@external(c, "", "gleam_string_unsafe_int_to_utf_codepoint")
fn unsafe_int_to_utf_codepoint(a: Int) -> UtfCodepoint

@external(c, "", "gleam_string_from_utf_codepoints")
pub fn from_utf_codepoints(utf_codepoints: List(UtfCodepoint)) -> String

@external(c, "", "gleam_string_do_to_utf_codepoints")
fn do_to_utf_codepoints(string: String) -> List(UtfCodepoint)

fn to_utf_codepoints_loop(
  bit_array: BitArray,
  acc: List(UtfCodepoint),
) -> List(UtfCodepoint) {
  panic as "should not be used"
}
