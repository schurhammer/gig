//// 11

import gleam/io

pub fn main() {
  let x = Ok(1)
  let x = case x {
    Ok(x) -> x
    Error(_) -> 2
  }
  let y = Error(10)
  let y = case y {
    Ok(_) -> 20
    Error(x) -> x
  }
  io.debug(x + y)
}
