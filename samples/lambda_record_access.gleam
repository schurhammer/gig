//// 2

type Foo {
  Foo(value: Int)
}

pub fn main() {
  bar(Foo(1), fn(x) -> Int { x.value + 1 })
  |> echo
}

fn bar(x: a, f: fn(a) -> Int) -> Int {
  f(x)
}
