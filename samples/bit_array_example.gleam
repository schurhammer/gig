//// 1
//// 1
//// 1
//// 1
//// 1
//// 1

import gleam/io

pub fn main() {
  let x = 1
  let y = 2
  let a = <<x, y, 0:4, 3:4>>
  case a {
    <<_, 2>> -> io.debug(3)
    <<3, 2, 1>> -> io.debug(4)
    <<a, 2, 3>> -> io.debug(a)
    _ -> io.debug(5)
  }
  let b = <<1, a:16-bits, a:bits>>
  case b {
    <<1, 1, 2, 1, 2, 3>> -> io.debug(1)
    _ -> io.debug(2)
  }
  let c = <<222, 1:3, 1:8, 1:5>>
  case c {
    <<222, 32, 33>> -> io.debug(1)
    _ -> io.debug(2)
  }
  let d = <<7, 8>>
  case d {
    <<a:16>> ->
      case a {
        1800 -> io.debug(1)
        _ -> io.debug(2)
      }
    _ -> io.debug(2)
  }
  let e = <<35>>
  case e {
    <<a:4, b:4>> -> io.debug(1)
    _ -> io.debug(2)
  }
  let f = <<1, 2, 3>>
  case f {
    <<a, rest:bits>> ->
      case rest {
        <<2, 3>> -> io.debug(1)
        _ -> {
          io.debug(3)
        }
      }
    _ -> io.debug(2)
  }
}
