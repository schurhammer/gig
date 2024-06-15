//// 3

import gleam/io

pub fn main() {
  io.debug(f(1) + f(2) + f(3))
}

fn f(n) {
  case n {
    1 | 2 -> n
    n -> 0
  }
}
