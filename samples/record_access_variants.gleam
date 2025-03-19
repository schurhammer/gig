//// "Bob"

import gleam/io

type Animal {
  Cat(name: String)
  Fox(name: String)
}

pub fn main() {
  let cat = Cat("Bob")
  io.debug(cat.name)
}
