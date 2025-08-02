pub fn bit_size(a: BitArray) -> Int {
  length_bit_array(a)
}

fn unsafe_to_string(a: BitArray) -> String {
  index_bit_array_string(a, 0, length_bit_array(a))
}
