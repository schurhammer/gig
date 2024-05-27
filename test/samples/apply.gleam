//// 3

pub fn main() {
  apply(inc, 2)
}

fn apply(f, x) {
  f(x)
}

fn inc(x) {
  x + 1
}
