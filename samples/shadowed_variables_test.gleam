//// one
//// two

import gleam/io

pub fn main() {
  let a = 1
  let a = "one"
  io.println(a)
  shadowed_param(1)
}

fn shadowed_param(b: Int) {
  let b = "two"
  io.println(b)
}
