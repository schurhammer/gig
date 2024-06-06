//// 6

pub fn main() {
  let x = 1
  let y = {
    let x = 2
    let y = 3
    x + y
  }
  print(inspect(x + y))
}
