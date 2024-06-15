//// 3

import gleam/io

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(1, 2)
  let Point(x, y) = point
  io.debug(x + y)
}
