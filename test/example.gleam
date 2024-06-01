import closure_conversion
import codegen
import glance
import gleam/io
import monomorphise
import typed

const input = "
//// 3

pub fn main() {
  let x = 1
  let add_x = fn(y: Bool) { x + y }
  add_x(2)
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
