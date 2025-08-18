//// 1
//// 1

// B is never constructed and as such List(Int) isn't either
// but we need to make sure the List(Int) type is still generated

type Foo {
  A(Int)
  B(List(Int))
}

pub fn main() {
  case A(1) {
    A(1) -> echo 1
    _ -> echo 2
  }
  let x = fn(x) { x }
  let y = fn(y) { x(y) }
  echo y(1)
}
