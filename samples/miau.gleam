import gleam/io
import gleam/string

pub fn main() {
  string.pop_grapheme("Hello, World!")
  |> io.debug()
}
