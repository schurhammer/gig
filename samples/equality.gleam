//// 21

import gleam/io

pub fn main() {
  let a = True
  let b = True
  let c = False
  let x = case a == b {
    True -> 1
    False -> 2
  }
  let y = case a == c {
    True -> 10
    False -> 20
  }
  io.debug(x + y)
}
