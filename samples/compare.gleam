//// False
//// True
//// False
//// False
//// False
//// True
//// False
//// False
//// True
//// False
//// True
//// False
//// True
//// False

type Point {
  Point(x: Int, y: Int)
}

type Foo {
  Bar(x: Int)
  Baz(x: Int)
}

pub fn main() {
  echo lt(1, 1)
  echo lt(1, 2)
  echo lt(2, 1)

  let a = Point(1, 2)
  let b = Point(1, 2)
  let c = Point(1, 3)

  echo lt(a, b)
  echo lt(b, a)
  echo lt(a, c)
  echo lt(c, a)

  let a = Bar(1)
  let b = Bar(2)
  let c = Baz(1)

  echo lt(a, a)
  echo lt(a, b)
  echo lt(b, a)
  echo lt(a, c)
  echo lt(c, a)
  echo lt(b, c)
  echo lt(c, b)
}
