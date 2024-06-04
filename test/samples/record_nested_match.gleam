//// 3

type Point {
  A(x: Int, y: Int)
  B(z: Int)
}

pub fn main() {
  let a = Ok(A(1, 2))
  case a {
    Ok(A(x, y)) -> x + y
    Ok(B(z)) -> z
    _ -> 0
  }
}
