import closure_conversion
import codegen
import glance
import gleam/io
import monomorphise
import typed

const input = "
//// 1

pub fn main() {
  a(10)
}

fn a(x) {
  case x {
    1 -> 1
    n -> b(n + 1)
  }
}

fn b(x) {
  a(x / 2)
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
