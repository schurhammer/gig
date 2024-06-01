//// 3

type PointT {
  Point(x: Int, y: Int)
}

pub fn main() {
  let point = Point(1, 2)
  sum(point)
}

fn sum(point: PointT) {
  point.x + point.y
}
