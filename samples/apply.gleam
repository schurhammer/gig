//// 3

import gleam/io

pub fn main() {
  io.debug(apply(inc, 2))
}

fn apply(f, x) {
  f(x)
}

fn inc(x) {
  x + 1
}
