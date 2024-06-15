//// 21

import gleam/io

pub fn main() {
  io.debug(fib(8))
}

fn fib(n) {
  case n {
    0 | 1 -> n
    n -> fib(n - 2) + fib(n - 1)
  }
}
