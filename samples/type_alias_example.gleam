////

type Foo(a) {
  Foo(a)
}

type FooInt =
  Foo(Int)

type Bar(a) =
  Foo(a)

type Baz(a) =
  #(a, a)

pub fn main() {
  foo(Foo("foo"))
  foo_int(Foo(1))
  bar(Foo("bar"))
  baz(#("a", "b"))
}

fn baz(x: Baz(String)) {
  x
}

fn foo(x: Foo(String)) {
  x
}

fn foo_int(x: FooInt) {
  x
}

fn bar(x: Bar(String)) {
  x
}
