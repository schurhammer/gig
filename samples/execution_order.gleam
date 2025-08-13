//// 0
//// 1
//// 2
//// 3
//// 4
//// 5
//// 6
//// 7
//// 1
//// 2
//// 3
//// 3
//// 2
//// 1

import gleam/io
import gleam/list

fn main() {
  foo(io.debug(0), io.debug(1), io.debug(2))
  let x = {
    io.debug(4)
    io.debug(5)
  }
  io.debug(6)
  io.debug(7)
  list.fold([1, 2, 3], Nil, fn(a, i) {
    io.debug(i)
    Nil
  })
  list.fold_right([1, 2, 3], Nil, fn(a, i) {
    io.debug(i)
    Nil
  })
}

fn foo(a, b, c) {
  io.debug(3)
  Nil
}
