//// 2

pub type Foo {
  Foo(a: Int, b: Int, c: Int)
}

pub fn main() {
  let foo = Foo(1, 2, 3)
  case foo {
    Foo(b:, ..) -> {
      echo b
    }
  }
}
