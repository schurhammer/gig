//// 0

pub fn main() {
  let x = 3
  case
    case x {
      1 -> 1
      x -> x
    }
  {
    1 -> 1
    2 -> 2
    n ->
      case n {
        3 -> 0
        n -> n
      }
  }
}
