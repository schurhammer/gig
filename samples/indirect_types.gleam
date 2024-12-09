//// 1
//// 1

import gleam/io

type Foo {
  A(Int)
  B(List(Int))
}

// B is never constructed and as such List(Int) isn't either
// but we need to make sure the code to do it is still generated
pub fn main() {
  case A(1) {
    A(1) -> io.debug(1)
    _ -> io.debug(2)
  }
  let x = fn(x) { x }
  let y = fn(y) { x(y) }
  io.debug(y(1))
}
