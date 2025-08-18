//// Bar(1, True)
//// 1
//// 1

type PointT {
  Point(x: Int, y: Bool)
}

type Foo {
  Bar(x: Int, y: Bool)
}

pub fn main() {
  echo Bar(1, True)
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
  echo x
  echo y
}
