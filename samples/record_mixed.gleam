//// Bar(1, True)
//// 1
//// 1

import gleam/io

type PointT {
  Point(x: Int, y: Bool)
}

type Foo {
  Bar(x: Int, y: Bool)
}

pub fn main() {
  io.debug(Bar(1, True))
  let point = Point(1, True)
  let x = case point.y {
    True -> point.x
    False -> 0
  }
  let Point(n, b) = point
  let y = case b {
    True -> n
    False -> 0
  }
  io.debug(x)
  io.debug(y)
}
