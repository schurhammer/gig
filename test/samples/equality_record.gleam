//// 21

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let a = Point(1, 2)
  let b = Point(1, 2)
  let c = Point(1, 3)
  let x = case a == b {
    True -> 1
    False -> 2
  }
  let y = case a == c {
    True -> 10
    False -> 20
  }
  x + y
}
