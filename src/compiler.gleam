import closure_conversion
import codegen
import glance
import monomorphise
import typed as t

import shellout
import simplifile

import gleam/io
import gleam/string

// returns the file name of the binary
pub fn compile(gleam_file_name: String, gc gc: Bool, release release: Bool) {
  io.debug(#(gleam_file_name, gc))
  let assert Ok(input) = simplifile.read(gleam_file_name)

  // run it through the compiler chain
  let assert Ok(module) = glance.module(input)
  let typed = t.infer_module(module)
  let mono = monomorphise.run(typed)
  let cc = closure_conversion.cc_module(mono)
  let code = codegen.module(cc)

  // insert the generated code into the template
  let assert Ok(template) = simplifile.read("./src/template.c")

  let template = case gc {
    True -> {
      let includes =
        "
        #include <gc.h>
        #define malloc(x) GC_MALLOC(x)
      "
      let init =
        "
        GC_INIT();
      "
      let template = string.replace(template, "///INIT///", init)
      let template = string.replace(template, "///INCLUDES///", includes)
      template
    }
    False -> template
  }

  let output = string.replace(template, "///CODEGEN_CONTENT///", code)

  // output the c file
  let assert [file_name, ..] = string.split(gleam_file_name, ".gleam")
  let c_file = file_name <> ".c"
  let assert Ok(_) = simplifile.write(c_file, output)

  // compile the c file
  let args = ["-o", file_name, c_file]

  let args = case gc {
    True -> ["-lgc", ..args]
    False -> args
  }

  let args = case release {
    True -> ["-O3", ..args]
    False -> args
  }

  io.debug(args)
  let result = shellout.command("gcc", args, ".", [])

  case result {
    Ok(_) -> Nil
    Error(message) -> io.println_error(message.1)
  }

  file_name
}
