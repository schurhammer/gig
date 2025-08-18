//// 1

pub fn main() {
  case Ok(1) {
    Ok(x) -> echo x
    _ -> echo 0
  }
}
