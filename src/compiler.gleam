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

const dependency_path = "./build/c/packages"

fn all_but_last(l: List(a)) -> List(a) {
  case l {
    [] | [_] -> []
    [x, ..xs] -> [x, ..all_but_last(xs)]
  }
}

fn read_file(name) {
  case simplifile.read(name) {
    Ok(content) -> content
    _ -> panic as { "Failed to read file " <> name }
  }
}

fn read_source_file(path, name) {
  case simplifile.read(path <> "/" <> name) {
    Ok(content) -> content
    _ ->
      case simplifile.read(dependency_path <> "/" <> name) {
        Ok(content) -> content
        _ -> panic as { "Failed to read module " <> name }
      }
  }
}

// returns the file name of the binary
pub fn compile(
  gleam_file_name: String,
  compiler compiler: String,
  gc gc: Bool,
  release release: Bool,
) {
  let split = string.split(gleam_file_name, "/")
  let assert Ok(file) = list.last(split)
  let path =
    all_but_last(split)
    |> string.join("/")

  let path = case path {
    "" -> "."
    _ -> path
  }

  // set up dependencies
  let _ = simplifile.delete(dependency_path)
  case simplifile.copy_directory("./stdlib/", dependency_path) {
    Ok(_) -> Nil
    _ -> panic as "failed to copy stdlib"
  }

  // process the prelude
  let prelude = read_file("stdlib/gleam.gleam")
  let assert Ok(prelude) = glance.module(prelude)
  let c = t.prelude_context()
  let c = t.infer_module(c, prelude)

  // parse and typecheck input (recursively)
  let #(typed, _done) = infer_file(c, ["gleam.gleam"], path, file)

  // generate code
  let mono = monomorphise.run(typed)
  let cc = closure_conversion.cc_module(mono)
  let code = codegen.module(cc)

  // insert the generated code into the template
  let template = read_file("./src/template.c")

  let module_name = string.replace(file, ".gleam", "")
  let main_call = "F_" <> module_name <> "_main();\n"

  let template = case gc {
    True -> {
      // TODO GC_enable_incremental
      // TODO GC_MALLOC_ATOMIC
      // TODO GC_set_pointer_mask(0x0000FFFFFFFFFFFF)
      let includes = "#define GC\n"
      let init = main_call
      let template = string.replace(template, "/// INIT", init)
      let template = string.replace(template, "/// INCLUDES", includes)
      template
    }
    False -> {
      let includes = ""
      let init = main_call
      let template = string.replace(template, "/// INIT", init)
      let template = string.replace(template, "/// INCLUDES", includes)
      template
    }
  }

  let output = string.replace(template, "/// CODEGEN", code)

  // output the c file
  let assert [file_name, ..] = string.split(gleam_file_name, ".gleam")
  let c_file = file_name <> ".c"
  io.println("Generating " <> c_file)
  case simplifile.write(c_file, output) {
    Ok(_) -> Nil
    _ -> panic as { "Failed to write file " <> c_file }
  }

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

  io.println("Generating binary " <> file_name)
  case shellout.command(compiler, args, ".", []) {
    Ok(_) -> Nil
    Error(message) ->
      io.println_error("Failed to generate binary:\n" <> message.1)
  }

  file_name
}

fn infer_file(
  c: t.Context,
  d: List(String),
  path: String,
  file: String,
) -> #(t.Context, List(String)) {
  io.println("Compiling " <> file)
  // add file to "done" list
  let d = [file, ..d]

  // parse file
  let input = read_source_file(path, file)
  let module = case glance.module(input) {
    Ok(mod) -> mod
    _ -> panic as { "Failed to parse " <> file }
  }

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
