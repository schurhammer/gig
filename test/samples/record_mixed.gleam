//// 2

type PointT {
  Point(x: Int, y: Bool)
}

pub fn main() {
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
  x + y
}
