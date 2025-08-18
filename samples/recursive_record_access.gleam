//// 1

import gleam/list

type Foo {
  Foo(bar: Int)
}

pub fn main() {
  echo rec(0).bar
}

fn rec(f: Int) -> Foo {
  case f {
    1 -> Foo(1)
    _ -> Foo(rec(1).bar)
  }
}
