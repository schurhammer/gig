//// 21

import gleam/io

type ConsList(a) {
  NullList
  ConsList(head: a, tail: ConsList(a))
}

pub fn main() {
  let a = ConsList(1, ConsList(2, ConsList(3, NullList)))
  let b = ConsList(1, ConsList(2, ConsList(3, NullList)))
  let c = ConsList(1, ConsList(2, NullList))
  let x = case a == b {
    True -> 1
    False -> 2
  }
  let y = case a == c {
    True -> 10
    False -> 20
  }
  io.debug(x + y)
}
