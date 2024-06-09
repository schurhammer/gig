import closure_conversion
import codegen
import glance
import monomorphise
import typed as t

import env
import shellout
import simplifile

import gleam/io
import gleam/list
import gleam/string

fn all_but_last(l: List(a)) -> List(a) {
  case l {
    [] | [_] -> []
    [x, ..xs] -> [x, ..all_but_last(xs)]
  }
}

// returns the file name of the binary
pub fn compile(gleam_file_name: String, gc gc: Bool, release release: Bool) {
  io.debug(#(gleam_file_name, gc))

  let split = string.split(gleam_file_name, "/")
  let assert Ok(file) = list.last(split)
  let path =
    all_but_last(split)
    |> string.join("/")

  let c = t.prelude_context()

  // let assert Ok(input) = simplifile.read(gleam_file_name)
  // run it through the compiler chain
  // let assert Ok(module) = glance.module(input)
  // let typed = t.infer_module(c, module)

  let #(typed, _done) = infer_file(c, [], path, file)
  let mono = monomorphise.run(typed)
  let cc = closure_conversion.cc_module(mono)
  let code = codegen.module(cc)

  // insert the generated code into the template
  let assert Ok(template) = simplifile.read("./src/template.c")

  let module_name = string.replace(file, ".gleam", "")
  let main_call = "F_" <> module_name <> "_main();\n"

  let template = case gc {
    True -> {
      let includes =
        "
        #include <gc.h>
        #define malloc(x) GC_MALLOC(x)
      "
      let init = "GC_INIT();\n" <> main_call
      let template = string.replace(template, "///INIT///", init)
      let template = string.replace(template, "///INCLUDES///", includes)
      template
    }
    False -> {
      let init = main_call
      let template = string.replace(template, "///INIT///", init)
      template
    }
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

fn infer_file(
  c: t.Context,
  d: List(String),
  path: String,
  file: String,
) -> #(t.Context, List(String)) {
  // add file to "done" list
  let d = [file, ..d]

  // parse file
  let assert Ok(input) = simplifile.read(path <> "/" <> file)
  let assert Ok(module) = glance.module(input)

  let module_name = string.replace(file, ".gleam", "")

  // infer imports
  let #(c, d, module_names) =
    list.fold(module.imports, #(c, d, env.new()), fn(acc, i) {
      let #(c, d, module_names) = acc
      let name = i.definition.module
      let file = name <> ".gleam"
      let assert Ok(alias) = list.last(string.split(name, "/"))
      let module_names = env.put(module_names, alias, name)
      let #(c, d) = infer_file(c, d, path, file)
      #(c, d, module_names)
    })

  let c = t.Context(..c, name: module_name, module_names: module_names)

  // infer this file
  let c = t.infer_module(c, module)

  #(c, d)
}
