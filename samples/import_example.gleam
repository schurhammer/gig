//// hello world

import gleam/io
import imported_example
import nested/module

pub fn main() {
  module.hello()
  io.print(" ")
  imported_example.world()
}
