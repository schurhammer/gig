//// 3

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(1, 2)
  print(inspect(point.x + point.y))
}
