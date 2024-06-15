//// 3

import gleam/io

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(1, 2)
  io.debug(sum(point))
}

fn sum(point: PointT) {
  point.x + point.y
}
