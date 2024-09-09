// TODO figure out the minimum required external functions to be able to
// implement all the string functionality
// maybe we can build on bitstring / dynamic types once its supported
import gleam
import gleam/order

@external(c, "", "append_String")
pub fn append(a: String, b: String) -> String

@external(c, "", "starts_with_String")
pub fn starts_with(string: String, with: String) -> Bool

@external(c, "", "ends_with_String")
pub fn ends_with(string: String, with: String) -> Bool

@external(c, "", "compare_String")
fn do_compare(a: String, b: String) -> Int

pub fn compare(a: String, b: String) -> order.Order {
    case do_compare(a, b) {
        x if x < 0 -> order.Lt
        x if x > 0 -> order.Gt
        _ -> order.Eq
    }
}

pub fn inspect(value: a) -> String {
    gleam.inspect(value)
}
