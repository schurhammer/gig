//// 5

import gleam/io

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(5, 6)
  let Point(y: _, x: x) = point
  io.debug(x)
}
