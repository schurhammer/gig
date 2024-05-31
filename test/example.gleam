import closure_conversion
import codegen
import glance
import gleam/io
import monomorphise
import typed

const input = "
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


"

pub fn main() {
  let assert Ok(m) = glance.module(input)
  let m = typed.infer_module(m)
  let m = monomorphise.run(m)
  let m = closure_conversion.cc_module(m)
  let c = codegen.module(m)
  io.println(c)
}
