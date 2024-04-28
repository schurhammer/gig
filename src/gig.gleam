import glance as g
import gleam/io

pub fn main() {
  g.module(
    "
  pub fn main() {
    let a = 1
    let b = 2
    a + b
  }
  ",
  )
  |> io.debug
}
