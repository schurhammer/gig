//// 3

pub fn main() {
  print(inspect(apply(inc, 2)))
}

fn apply(f, x) {
  f(x)
}

fn inc(x) {
  x + 1
}
