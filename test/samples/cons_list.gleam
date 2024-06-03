//// 6

type ConsList(a) {
  NullList
  ConsList(head: a, tail: ConsList(a))
}

pub fn main() {
  sum(range(3))
}

fn sum(l) {
  case l {
    NullList -> 0
    ConsList(x, xs) -> x + sum(xs)
  }
}

fn range(i) {
  case i {
    0 -> NullList
    i -> ConsList(i, range(i - 1))
  }
}
