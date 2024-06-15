//// 6

import gleam/io

pub fn main() {
  let x = 1
  let y = {
    let x = 2
    let y = 3
    x + y
  }
  io.debug(x + y)
}
