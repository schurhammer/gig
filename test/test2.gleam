import glance
import typed2

const input = "
  fn main() {
    id(1)
  }
  fn id(x) {
    x
  }
  fn inc(n) {
    n + 1
  }
  fn fact(n) {
    n * fact(n - 1)
  }
"

pub fn main() {
  let assert Ok(m) = glance.module(input)
  typed2.infer_module(m)
}
