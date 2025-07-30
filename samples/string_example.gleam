//// hello world
//// Ok(#("h", "ello"))

import gleam/io
import gleam/string

pub fn main() {
  let x = "hello"
  let y = "world"
  io.println(x <> " " <> y)
  io.debug(string.pop_grapheme(x))
}
