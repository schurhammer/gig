//// 1
//// 2
//// 1
//// 2
//// 1
//// 2
//// 5
//// 6

pub fn main() {
  use <- simple
  use <- simple()
  use <- args(1, 2)
  use a, b <- continuation_args
  let a = a
  let b = b
  echo a
  echo b
  use a, b <- continuation_args()
  echo a
  echo b
  use a, b <- complex(2, 3)
  echo a
  echo b
}

fn simple(f: fn() -> Int) {
  f()
}

fn args(a: Int, b: Int, f: fn() -> Int) {
  echo a
  echo b
  f()
}

fn continuation_args(f: fn(Int, Int) -> Int) {
  f(1, 2)
}

fn complex(a: Int, b: Int, f: fn(Int, Int) -> Int) {
  f(a + b, a * b)
}
