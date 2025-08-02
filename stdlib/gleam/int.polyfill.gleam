@external(c, "", "gleam_int_to_string")
pub fn to_string(x: Int) -> String

@external(c, "", "gleam_int_parse")
pub fn parse(string: String) -> Result(Int, Nil)

@external(c, "", "gleam_int_do_base_parse")
fn do_base_parse(a: String, b: Int) -> Result(Int, Nil)

@external(c, "", "gleam_int_bitwise_and")
pub fn bitwise_and(x: Int, y: Int) -> Int
