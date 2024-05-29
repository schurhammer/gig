import glance
import monomorphise
import typed

const input = "

pub fn main() {
  fib(8)
}

fn fib(n) {
  case n {
    0 | 1 -> n
    n -> fib(n - 2) + fib(n - 1)
  }
}

  // fn a() {
  //   b()
  // }
  // fn b() {
  //   a()
  // }
  // fn main() {
  //   id(id)(1)
  //   id(1)
  // }
  // fn id(x) {
  //   x
  // }
  // fn inc(n) {
  //   n + 1
  // }
  // fn fact(n) {
  //   n * fact(n - 1)
  // }
  // fn lambda_example() {
  //   let plus_one = fn(x) {
  //     x + 1
  //   }
  //   plus_one(2)
  // }
  // fn let_example() {
  //   let x = 1
  //   let y = 2
  //   x + y
  // }
  // fn case_example(n) {
  //   case n {
  //     1 | 2 -> n
  //     n -> 0
  //   }
  // }
"

pub fn main() {
  let assert Ok(m) = glance.module(input)
  let m = typed.infer_module(m)
  monomorphise.run(m)
}
