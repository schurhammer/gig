//// 1
//// 2
//// 3
//// 1
//// 4
//// 3

type Point {
  Point(x: Int, y: Int, z: Int)
}

pub fn main() {
  let a = Point(1, 2, 3)
  let b = Point(..a, y: 4)
  echo a.x
  echo a.y
  echo a.z
  echo b.x
  echo b.y
  echo b.z
}
