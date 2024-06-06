//// 2

pub fn main() {
  let x = case !True {
    True -> 1
    False -> 2
  }
  print(inspect(x))
}
