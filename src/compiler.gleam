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
pub fn compile(gleam_file_name: String) {
  let assert Ok(input) = simplifile.read(gleam_file_name)

  // run it through the compiler chain
  let assert Ok(module) = glance.module(input)
  let typed = t.infer_module(module)
  let mono = monomorphise.run(typed)
  let cc = closure_conversion.cc_module(mono)
  let code = codegen.module(cc)

  // insert the generated code into the template
  let assert Ok(template) = simplifile.read("./src/template.c")
  let output = string.replace(template, "///CODEGEN_CONTENT///", code)

  // output the c file
  let assert [file_name, ..] = string.split(gleam_file_name, ".gleam")
  let c_file = file_name <> ".c"
  let cmd = "echo '" <> output <> "' > " <> c_file
  let assert Ok(_) = shellout.command("bash", ["-c", cmd], ".", [])

  // compile the c file
  let result = shellout.command("gcc", ["-o", file_name, c_file], ".", [])

  case result {
    Ok(_) -> Nil
    Error(message) -> io.println_error(message.1)
  }

  file_name
}
