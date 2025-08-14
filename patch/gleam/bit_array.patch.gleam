pub fn bit_size(a: BitArray) -> Int {
  length_bit_array(a)
}

fn unsafe_to_string(a: BitArray) -> String {
  index_bit_array_string(a, 0, length_bit_array(a))
}

@external(c, "", "gleam_bit_array_from_string")
fn from_string(x: String) -> BitArray

fn is_utf8_loop(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    <<_:utf8, rest:bytes>> -> is_utf8_loop(rest)
    _ -> False
  }
}
