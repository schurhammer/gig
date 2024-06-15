//// 6

import gleam/io

type Box(a) {
  Box(value: a)
}

pub fn main() {
  let box = Box(6)
  io.debug(box.value)
}
