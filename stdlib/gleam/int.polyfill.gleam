@external(c, "", "gleam_int_to_string")
pub fn to_string(x: Int) -> String

@external(c, "", "gleam_int_parse")
pub fn parse(string: String) -> Result(Int, Nil)

@external(c, "", "gleam_int_do_base_parse")
fn do_base_parse(a: String, b: Int) -> Result(Int, Nil)

@external(c, "", "gleam_int_bitwise_and")
pub fn bitwise_and(x: Int, y: Int) -> Int

@external(c, "", "gleam_int_bitwise_exclusive_or")
pub fn bitwise_exclusive_or(x: Int, y: Int) -> Int

@external(c, "", "gleam_int_bitwise_shift_left")
pub fn bitwise_shift_left(x: Int, y: Int) -> Int

@external(c, "", "gleam_int_bitwise_or")
pub fn bitwise_or(x: Int, y: Int) -> Int

@external(c, "", "gleam_int_bitwise_not")
pub fn bitwise_not(x: Int) -> Int

@external(c, "", "gleam_int_to_float")
pub fn to_float(x: Int) -> Float

@external(c, "", "gleam_int_bitwise_shift_right")
pub fn bitwise_shift_right(x: Int, y: Int) -> Int

@external(c, "", "gleam_int_do_to_base_string")
pub fn do_to_base_string(a: Int, b: Int) -> String
