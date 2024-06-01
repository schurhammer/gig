//// 21

type List(a) {
  Null
  Cons(head: a, tail: List(a))
}

pub fn main() {
  let a = Cons(1, Cons(2, Cons(3, Null)))
  let b = Cons(1, Cons(2, Cons(3, Null)))
  let c = Cons(1, Cons(2, Null))
  let x = case a == b {
    True -> 1
    False -> 2
  }
  let y = case a == c {
    True -> 10
    False -> 20
  }
  x + y
}
