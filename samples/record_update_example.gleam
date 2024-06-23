//// 1
//// 2
//// 3
//// 1
//// 4
//// 3

import gleam/io

type Point {
  Point(x: Int, y: Int, z: Int)
}

pub fn main() {
  let a = Point(1, 2, 3)
  let b = Point(..a, y: 4)
  io.debug(a.x)
  io.debug(a.y)
  io.debug(a.z)
  io.debug(b.x)
  io.debug(b.y)
  io.debug(b.z)
}
