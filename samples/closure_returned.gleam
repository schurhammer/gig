//// 7

import gleam/io

pub fn main() {
  let add5 = make_adder(5)
  io.debug(add5(2))
}

pub fn make_adder(x) {
  fn(y) { x + y }
}
