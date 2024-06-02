//// 21

type Point {
  Point(x: Int, y: Int)
}

type List(a) {
  Null
  Cons(head: a, tail: List(a))
}

pub fn main() {
  let points = Cons(Point(1, 2), Cons(Point(3, 4), Cons(Point(5, 6), Null)))
  let ints = map(points, fn(point: Point) { point.x + point.y })
  sum(ints)
}

fn map(l, f) {
  case l {
    Null -> Null
    Cons(a, b) -> Cons(f(a), map(b, f))
  }
}

fn sum(l) {
  case l {
    Null -> 0
    Cons(x, xs) -> x + sum(xs)
  }
}
