//// 3

import gleam/io

fn add(x, y) {
  x + y
}

pub fn main() {
  let add1 = add(1, _)
  io.debug(add1(2))
}
