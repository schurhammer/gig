//// 6

type List(a) {
  Null
  Cons(head: a, tail: List(a))
}

pub fn main() {
  let _ = Cons(1, Cons(2, Cons(3, Null)))
  sum(range(3))
}

fn sum(l) {
  case l {
    Null -> 0
    Cons(x, xs) -> x + sum(xs)
  }
}

fn range(i) {
  case i {
    0 -> Null
    i -> Cons(i, range(i - 1))
  }
}
