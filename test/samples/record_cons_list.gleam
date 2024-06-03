//// 21

type Point {
  Point(x: Int, y: Int)
}

type ConsList(a) {
  NullList
  ConsList(head: a, tail: ConsList(a))
}

pub fn main() {
  let points =
    ConsList(
      Point(1, 2),
      ConsList(Point(3, 4), ConsList(Point(5, 6), NullList)),
    )
  let ints = map(points, fn(point: Point) { point.x + point.y })
  sum(ints)
}

fn map(l, f) {
  case l {
    NullList -> NullList
    ConsList(a, b) -> ConsList(f(a), map(b, f))
  }
}

fn sum(l) {
  case l {
    NullList -> 0
    ConsList(x, xs) -> x + sum(xs)
  }
}
