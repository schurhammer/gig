//// 1

import gleam/io

fn sub(x x, y y) {
  x - y
}

fn foo(a a: Int, b b: String) {
  Nil
}

pub fn main() {
  // make sure this type checks both ways
  foo(a: 1, b: "bar")
  foo(b: "bar", a: 1)

  // make sure it is really applied in the correct order
  io.debug(sub(y: 1, x: 2))
}
