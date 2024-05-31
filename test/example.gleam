import closure_conversion
import codegen
import glance
import gleam/io
import monomorphise
import typed

const input = "
//// 6

type Box(a) {
  Box(value: a)
}

pub fn main() {
  let box = Box(6)
  box.value
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
