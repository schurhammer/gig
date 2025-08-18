pub type Nil {
  Nil
}

pub type Bool {
  False
  True
}

pub type Int

pub type Float

pub type String

pub type BitArray

pub type UtfCodepoint

pub type Result(a, b) {
  Ok(value: a)
  Error(value: b)
}

pub type List(a) {
  Empty
  Cons(item: a, next: List(a))
}

// Auto Generated Functions

@builtin()
@external(c, "", "eq")
pub fn eq(a: a, b: a) -> Bool

@builtin()
@external(c, "", "lt")
pub fn lt(a: a, b: a) -> Bool

@builtin()
@external(c, "", "inspect")
pub fn inspect(a: a) -> String

// Bool Operators

@external(c, "", "and_bool")
pub fn and_bool(a: Bool, b: Bool) -> Bool

@external(c, "", "or_bool")
pub fn or_bool(a: Bool, b: Bool) -> Bool

@external(c, "", "negate_bool")
pub fn negate_bool(b: Bool) -> Bool

// Int Operators

@external(c, "", "lt_Int")
pub fn lt_int(a: Int, b: Int) -> Bool

@external(c, "", "gt_Int")
pub fn gt_int(a: Int, b: Int) -> Bool

@external(c, "", "lte_Int")
pub fn lte_int(a: Int, b: Int) -> Bool

@external(c, "", "gte_Int")
pub fn gte_int(a: Int, b: Int) -> Bool

@external(c, "", "add_Int")
pub fn add_int(a: Int, b: Int) -> Int

@external(c, "", "sub_Int")
pub fn sub_int(a: Int, b: Int) -> Int

@external(c, "", "mul_Int")
pub fn mul_int(a: Int, b: Int) -> Int

@external(c, "", "div_Int")
pub fn div_int(a: Int, b: Int) -> Int

@external(c, "", "rem_Int")
pub fn rem_int(a: Int, b: Int) -> Int

@external(c, "", "negate_Int")
pub fn negate_int(a: Int) -> Int

// Float Operators

@external(c, "", "lt_Float")
pub fn lt_float(a: Float, b: Float) -> Bool

@external(c, "", "gt_Float")
pub fn gt_float(a: Float, b: Float) -> Bool

@external(c, "", "lte_Float")
pub fn lte_float(a: Float, b: Float) -> Bool

@external(c, "", "gte_Float")
pub fn gte_float(a: Float, b: Float) -> Bool

@external(c, "", "add_Float")
pub fn add_float(a: Float, b: Float) -> Float

@external(c, "", "sub_Float")
pub fn sub_float(a: Float, b: Float) -> Float

@external(c, "", "mul_Float")
pub fn mul_float(a: Float, b: Float) -> Float

@external(c, "", "div_Float")
pub fn div_float(a: Float, b: Float) -> Float

// String Operators

@external(c, "", "length_string")
pub fn length_string(a: String) -> Int

@external(c, "", "append_string")
pub fn append_string(a: String, b: String) -> String

@external(c, "", "starts_with_string")
pub fn starts_with_string(string: String, prefix: String) -> Bool

@external(c, "", "drop_start_string")
pub fn drop_start_string(string: String, count: Int) -> String

// BitArray Operators

@external(c, "", "write_bit_array_string")
pub fn write_bit_array_string(value: String, to: BitArray, at_offset: Int, len: Int) -> Nil

@external(c, "", "write_bit_array_int")
pub fn write_bit_array_int(value: Int, to: BitArray, at_offset: Int, len: Int) -> Nil

@external(c, "", "write_bit_array")
pub fn write_bit_array(value: BitArray, to: BitArray, to_offset: Int, len: Int) -> Nil

@external(c, "", "index_bit_array_string")
pub fn index_bit_array_string(a: BitArray, offset: Int, length: Int) -> String

@external(c, "", "index_bit_array_int")
pub fn index_bit_array_int(a: BitArray, offset: Int, length: Int) -> Int

@external(c, "", "slice_bit_array")
pub fn slice_bit_array(a: BitArray, offset: Int, length: Int) -> BitArray

@external(c, "", "length_bit_array")
pub fn length_bit_array(a: BitArray) -> Int

// Other Functions

@external(c, "", "panic_exit")
pub fn panic_exit(message: String) -> a

@external(c, "", "print_string")
pub fn print_string(message: String) -> Nil

pub fn echo_(value: a) -> a {
  print_string(inspect(value))
  print_string("\n")
  value
}
