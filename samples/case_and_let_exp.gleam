//// 1
//// 1
//// 1
//// 1

import gleam/io

pub fn main() {
  io.debug(case 1 {
    n -> n
  })
  io.debug({
    let x = 1
    x
  })
  case 1 {
    n -> n
  }
  |> io.debug()
  {
    let x = 1
    x
  }
  |> io.debug()
}
