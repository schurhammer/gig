pub type Nil {
  Nil
}

pub type Bool

pub type Int

pub type Float

pub type String

pub type Result(a, b) {
  Ok(value: a)
  Error(value: b)
}

pub type List(a) {
  Empty
  Cons(item: a, next: List(a))
}

// Bool Operators

@external(c, "", "and_bool")
pub fn and_bool(a: Bool, b: Bool) -> Bool

@external(c, "", "or_bool")
pub fn or_bool(a: Bool, b: Bool) -> Bool

@external(c, "", "negate_bool")
pub fn negate_bool(b: Bool) -> Bool

// Int Operators

@external(c, "", "lt_int")
pub fn lt_int(a: Int, b: Int) -> Bool

@external(c, "", "gt_int")
pub fn gt_int(a: Int, b: Int) -> Bool

@external(c, "", "lte_int")
pub fn lte_int(a: Int, b: Int) -> Bool

@external(c, "", "gte_int")
pub fn gte_int(a: Int, b: Int) -> Bool

@external(c, "", "add_int")
pub fn add_int(a: Int, b: Int) -> Int

@external(c, "", "sub_int")
pub fn sub_int(a: Int, b: Int) -> Int

@external(c, "", "mul_int")
pub fn mul_int(a: Int, b: Int) -> Int

@external(c, "", "div_int")
pub fn div_int(a: Int, b: Int) -> Int

@external(c, "", "rem_int")
pub fn rem_int(a: Int, b: Int) -> Int

@external(c, "", "negate_int")
pub fn negate_int(a: Int) -> Int

// Float Operators

@external(c, "", "lt_float")
pub fn lt_float(a: Float, b: Float) -> Bool

@external(c, "", "gt_float")
pub fn gt_float(a: Float, b: Float) -> Bool

@external(c, "", "lte_float")
pub fn lte_float(a: Float, b: Float) -> Bool

@external(c, "", "gte_float")
pub fn gte_float(a: Float, b: Float) -> Bool

@external(c, "", "add_float")
pub fn add_float(a: Float, b: Float) -> Float

@external(c, "", "sub_float")
pub fn sub_float(a: Float, b: Float) -> Float

@external(c, "", "mul_float")
pub fn mul_float(a: Float, b: Float) -> Float

@external(c, "", "div_float")
pub fn div_float(a: Float, b: Float) -> Float

// String Operators

@external(c, "", "append_String")
pub fn append_string(a: String, b: String) -> String

// Other Functions

@external(c, "", "panic_exit")
pub fn panic_exit() -> a
