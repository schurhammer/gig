//// 1
//// 1
//// 2
//// 2
//// 1
//// 2
//// 3

import gleam/io

pub fn main() {
  f(1)
  f(2)
  f(3)
  g([1, 2, 3, 4, 5])
  h(Baz(1))
  h(Baz(2))
  h(Bar)
}

fn f(n) {
  // Test alternative patterns
  case n {
    1 | 2 -> io.debug(1)
    n -> io.debug(2)
  }
}

fn g(n) {
  // Test that "as" works correctly
  case n {
    [1, 2, 0 as x, ..rest] -> io.debug(1)
    [1, 2, ..rest] -> io.debug(2)
  }
}

type Foo {
  Bar
  Baz(val: Int)
}

fn h(n) {
  // Test that the guard is evaluated after the type check
  case n {
    Baz(var) if var == 1 -> io.debug(1)
    Baz(var) -> io.debug(2)
    Bar -> io.debug(3)
  }
}
