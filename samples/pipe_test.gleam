//// 5

import gleam/io

pub fn main() {
  10
  |> sub(3)
  |> sub(2)
  |> io.debug
}

fn sub(a, b) {
  a - b
}
