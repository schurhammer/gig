//// 1

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(y: 1, x: 2)
  point.x - point.y
}
