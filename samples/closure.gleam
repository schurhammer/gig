//// 3

import gleam/io

pub fn main() {
  let x = 1
  let add_x = fn(y) { x + y }
  io.debug(add_x(2))
}
