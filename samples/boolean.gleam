//// 2

import gleam/io

pub fn main() {
  let x = case !True {
    True -> 1
    False -> 2
  }
  io.debug(x)
}
