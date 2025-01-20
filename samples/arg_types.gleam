type Foo(x) {
  Foo(x: x)
}

fn foo(a: Int) -> Int {
  a + 1
}

fn bar(a: a) -> Foo(a) {
  Foo(a)
}

fn baz(f: fn(b) -> a, g: fn(a) -> Foo(a)) -> Int {
  1
}

pub fn main() {
  baz(foo, bar)
}
