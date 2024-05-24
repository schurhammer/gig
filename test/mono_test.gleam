import core
import gig
import glance

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
        id(1)
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
  Nil
}
