//// 1

pub fn main() {
  case 1 < 2 {
    True -> 1
    False ->
      case 1 < 2 {
        True -> todo
        False -> panic
      }
  }
}
