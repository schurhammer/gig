import ast
import closure_conversion
import codegen
import core
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
      fn fact(n) {
        case n {
          0 | 1 -> 1
          n -> n * fact(n - 1)
        }
      }
      fn id(x) {
        x
      }
      fn apply(f, x) {
        f(x)
      }
      fn main() {
        let x = 1
        let id_var = id
        let add_x = fn(y) { y + x }
        id_var(add_x(2))
        apply(fact, 3)
      }
  ",
    )
  io.debug(module)

  io.println_error("\nCORE\n")
  let core = ast.module_to_core(module)
  io.debug(core)

  // io.println_error("\nPOLY\n")
  // let assert Ok(module) = core.w_module(dict.from_list(ast.prelude), core)
  // list.each(module.functions, fn(fun) { pprint.debug(fun) })

  // io.println_error("\nMONO\n")
  // let module = monomorphise.run(module)
  // list.each(module.functions, fn(fun) { pprint.debug(fun) })

  // io.println_error("\nCLOSURES\n")
  // let module = closure_conversion.cc_module(module)
  // list.each(module.functions, fn(fun) { pprint.debug(fun) })
  // list.each(module.types, fn(typ) { pprint.debug(typ) })

  // io.println_error("\nCODEGEN\n")
  // let output = codegen.module(module)
  // io.println_error(output)

  Nil
}
