//// 3

import gleam/io

pub fn main() {
  let sub = fn(x, y) { x - y }
  io.debug(sub(5, 2))
}
