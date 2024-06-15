//// 3

import gleam/io

type Point {
  A(x: Int, y: Int)
  B(z: Int)
}

pub fn main() {
  let a = Ok(A(1, 2))
  let r = case a {
    Ok(A(x, y)) -> x + y
    Ok(B(z)) -> z
    _ -> 0
  }
  io.debug(r)
}
