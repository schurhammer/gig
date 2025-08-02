//// 1
//// 1
//// 2
//// 2

import gleam/io

pub fn main() {
  f(1)
  f(2)
  f(3)
  g([1, 2, 3, 4, 5])
}

fn f(n) {
  case n {
    1 | 2 -> io.debug(1)
    n -> io.debug(2)
  }
}

fn g(n) {
  case n {
    [1, 2, 0 as x, ..rest] -> io.debug(1)
    [1, 2, ..rest] -> io.debug(2)
  }
}
