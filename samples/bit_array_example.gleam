//// 1
//// 1
//// 1
//// 1
//// 1
//// 1
//// 261
//// 99
//// eq
//// neq

import gleam/io

pub fn main() {
  let x = 1
  let y = 2
  let a = <<x, y, 0:4, 3:4>>
  case a {
    <<_, 2>> -> echo 3
    <<3, 2, 1>> -> echo 4
    <<a, 2, 3>> -> echo a
    _ -> echo 5
  }
  let b = <<1, a:16-bits, a:bits>>
  case b {
    <<1, 1, 2, 1, 2, 3>> -> echo 1
    _ -> echo 2
  }
  let c = <<222, 1:3, 1:8, 1:5>>
  case c {
    <<222, 32, 33>> -> echo 1
    _ -> echo 2
  }
  let d = <<7, 8>>
  case d {
    <<a:16>> ->
      case a {
        1800 -> echo 1
        _ -> echo 2
      }
    _ -> echo 2
  }
  let e = <<35>>
  case e {
    <<a:4, b:4>> -> echo 1
    _ -> echo 2
  }
  let f = <<1, 2, 3>>
  case f {
    <<a, rest:bits>> ->
      case rest {
        <<2, 3>> -> echo 1
        _ -> {
          echo 3
        }
      }
    _ -> echo 2
  }

  let g = <<1, 2, 3>>
  let x = 16
  case g {
    <<a:size(x), b>> -> echo a + b
    _ -> echo 0
  }
  let f = <<"abc":utf8>>
  case f {
    <<"ab":utf8, c>> -> echo c
    _ -> echo 0
  }
  case <<1, 2, 3>> == <<1, 2, 3>> {
    True -> io.println("eq")
    False -> io.println("neq")
  }
  case <<1, 2, 3>> == <<3, 2, 1>> {
    True -> io.println("eq")
    False -> io.println("neq")
  }
}
