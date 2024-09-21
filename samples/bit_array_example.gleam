//// 3

import gleam/io

pub fn main() {
  let a = 1
  let b = 2
  let x = case <<a, b, 3>> {
    // wrong size
    <<_, 2>> -> 1
    // wrong numbers
    <<3, 2, 1>> -> 2
    // just right
    <<a, 2, _>> -> 3
    // too far
    _ -> 4
  }
  io.debug(x)
}
