//// 1

import gleam/io

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(y: 1, x: 2)
  io.debug(point.x - point.y)
}
