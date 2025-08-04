@external(c, "", "gleam_float_exponential")
pub fn exponential(x: Float) -> Float

@external(c, "", "gleam_float_do_power")
fn do_log(x: Float) -> Float

@external(c, "", "gleam_float_random")
pub fn random() -> Float

@external(c, "", "gleam_float_power")
fn do_power(a: Float, b: Float) -> Float

@external(c, "", "gleam_float_do_to_float")
fn do_to_float(a: Int) -> Float

@external(c, "", "gleam_float_truncate")
pub fn truncate(x: Float) -> Int

@external(c, "", "gleam_float_js_round")
fn js_round(a: Float) -> Int

@external(c, "", "gleam_float_floor")
pub fn floor(x: Float) -> Float

@external(c, "", "gleam_float_ceiling")
pub fn ceiling(x: Float) -> Float

@external(c, "", "gleam_float_to_string")
pub fn to_string(x: Float) -> String

@external(c, "", "gleam_float_parse")
pub fn parse(string: String) -> Result(Float, Nil)
