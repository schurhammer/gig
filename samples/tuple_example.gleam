//// 9

import gleam/io

pub fn main() {
  let p = #(1, 2, True)
  let x = case p.2 {
    True -> p.0 + p.1 + 3
    False -> p.0 + p.1 + 4
  }
  let #(y, z, _) = p
  io.debug(x + y + z)
}
