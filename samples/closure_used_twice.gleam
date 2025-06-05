//// 4

import gleam/io

pub fn main() {
  // testing using the closure variable twice
  let x = 1
  let add_x = fn(y) { x + x + y }
  io.debug(add_x(2))
}
