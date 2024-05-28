import glance
import typed2

const input = "
  fn id(x) {
    x
  }
  fn main() {
    id(1)
  }
"

pub fn main() {
  let assert Ok(m) = glance.module(input)
  typed2.infer_module(m)
}
