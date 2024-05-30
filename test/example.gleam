import closure_conversion
import codegen
import glance
import gleam/io
import monomorphise
import typed

const input = "

pub fn main() {
  a(1)
}

"

pub fn main() {
  let assert Ok(m) = glance.module(input)
  let m = typed.infer_module(m)
  let m = monomorphise.run(m)
  let m = closure_conversion.cc_module(m)
  let c = codegen.module(m)
  io.debug(c)
}
