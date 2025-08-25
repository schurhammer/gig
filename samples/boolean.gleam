//// False
//// 2

pub fn main() {
  echo !False && False
  let x = case !True {
    True -> 1
    False -> 2
  }
  echo x
}
