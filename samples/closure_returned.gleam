//// 7

pub fn main() {
  let add5 = make_adder(5)
  print(inspect(add5(2)))
}

pub fn make_adder(x) {
  fn(y) { x + y }
}
