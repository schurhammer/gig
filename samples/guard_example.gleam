//// 11

import gleam/io

pub fn main() {
  case 3 {
    n if n < 5 -> io.print("1")
    _ -> io.print("0")
  }
  case 8 {
    n if n < 5 -> io.print("0")
    _ -> io.print("1")
  }
}
