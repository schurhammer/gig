//// 6

type Box(a) {
  Box(value: a)
}

pub fn main() {
  let box = Box(6)
  echo box.value
}
