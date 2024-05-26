import closure_conversion
import codegen
import core
import gig
import glance
import monomorphise

import gleam/dict
import gleam/io
import gleam/list
import pprint

pub fn main() {
  io.println_error("\nAST\n")
  let assert Ok(module) =
    glance.module(
      "
      fn id(x) {
        x
      }
      fn main() {
        let x = 1
        let add_x = fn(y) { y + x }
        id(1)
        id(add_x(1))
      }
  ",
    )
  io.debug(module)

  io.println_error("\nCORE\n")
  let core = gig.module_to_core(module)
  io.debug(core)

  io.println_error("\nPOLY\n")
  let assert Ok(module) = core.w_module(dict.from_list(gig.prelude), core)
  list.each(module.functions, fn(fun) { pprint.debug(fun) })

  io.println_error("\nMONO\n")
  let module = monomorphise.run(module)
  list.each(module.functions, fn(fun) { pprint.debug(fun) })

  io.println_error("\nCLOSURES\n")
  let module = closure_conversion.cc_module(module)
  list.each(module.functions, fn(fun) { pprint.debug(fun) })
  list.each(module.types, fn(typ) { pprint.debug(typ) })

  io.println_error("\nCODEGEN\n")
  let output = codegen.module(module)
  io.println_error(output)

  Nil
}
