//// 1

import gleam/io

pub fn main() {
  case Ok(1) {
    Ok(x) -> io.debug(x)
    _ -> io.debug(0)
  }
}
