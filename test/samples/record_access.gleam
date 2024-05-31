//// 3

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(1, 2)
  point.x + point.y
}
// TODO below the type annotation is the only clue that it's a point
// fn sum(point: PointT) {
//   point.x + point.y
// }
