//// 21

pub fn main() {
  echo fib(8)
}

fn fib(n) {
  case n {
    0 | 1 -> n
    n -> fib(n - 2) + fib(n - 1)
  }
}
