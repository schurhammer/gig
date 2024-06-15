//// 1

import gleam/io

pub fn main() {
  io.debug(a(10))
}

fn a(x) {
  case x {
    1 -> 1
    n -> b(n + 1)
  }
}

fn b(x) {
  a(x / 2)
}
