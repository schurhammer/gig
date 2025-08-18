//// 1
//// 1
//// 2
//// 2
//// 1
//// 2
//// 3

pub fn main() {
  f(1)
  f(2)
  f(3)
  g([1, 2, 3, 4, 5])
  h(Baz(1))
  h(Baz(2))
  h(Bar)
}

fn f(n) {
  // Test alternative patterns
  case n {
    1 | 2 -> echo 1
    n -> echo 2
  }
}

fn g(n) {
  // Test that "as" works correctly
  case n {
    [1, 2, 0 as x, ..rest] -> echo 1
    [1, 2, ..rest] -> echo 2
  }
}

type Foo {
  Bar
  Baz(val: Int)
}

fn h(n) {
  // Test that the guard is evaluated after the type check
  case n {
    Baz(var) if var == 1 -> echo 1
    Baz(var) -> echo 2
    Bar -> echo 3
  }
}
