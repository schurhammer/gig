//// 1
//// 2
//// 1
//// 2
//// 1
//// 2
//// 5
//// 6

import gleam/io

pub fn main() {
  use <- simple
  use <- simple()
  use <- args(1, 2)
  use a, b <- continuation_args
  let a = a
  let b = b
  io.debug(a)
  io.debug(b)
  use a, b <- continuation_args()
  io.debug(a)
  io.debug(b)
  use a, b <- complex(2, 3)
  io.debug(a)
  io.debug(b)
}

fn simple(f: fn() -> Int) {
  f()
}

fn args(a: Int, b: Int, f: fn() -> Int) {
  io.debug(a)
  io.debug(b)
  f()
}

fn continuation_args(f: fn(Int, Int) -> Int) {
  f(1, 2)
}

fn complex(a: Int, b: Int, f: fn(Int, Int) -> Int) {
  f(a + b, a * b)
}
