import anf/codegen
import gig/closure
import gig/core
import gig/headers
import gig/mono
import gig/polyfill
import gig/typed_ast
import gleam/dict
import gleam/set

import gleam/int
import gleam/result

import glance
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

fn read_source(sources: dict.Dict(String, String), name: String) {
  case dict.get(sources, name) {
    Ok(file) ->
      case simplifile.read(file) {
        Ok(content) -> Ok(content)
        _ -> Error("Failed to read source file " <> file)
      }
    _ -> Error("No source for module " <> name)
  }
}

fn include_sources(
  sources: dict.Dict(String, String),
  src_dir: String,
) -> dict.Dict(String, String) {
  io.println("Sources " <> src_dir)
  let assert Ok(files) = simplifile.get_files(src_dir)

  files
  |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
  |> list.fold(sources, fn(sources, src) {
    let module =
      src
      |> string.replace(src_dir, "")
      |> string.replace(".gleam", "")
    dict.insert(sources, module, src)
  })
}

// returns the file name of the binary
pub fn compile(
  gleam_file_name: String,
  compiler compiler: String,
  gc gc: Bool,
  release release: Bool,
  debug debug: Bool,
) {
  let split = string.split(gleam_file_name, "/")
  let assert Ok(target_file) = list.last(split)
  let module_id = string.replace(target_file, ".gleam", "")

  let target_path =
    all_but_last(split)
    |> string.join("/")

  let target_path = case target_path {
    "" -> "./"
    _ -> target_path <> "/"
  }

  // create a lookup table for package sources
  let packages_dir = "./build/packages"
  let assert Ok(packages) = simplifile.read_directory(packages_dir)
  let sources =
    list.fold(packages, dict.new(), fn(sources, package) {
      let src_dir = packages_dir <> "/" <> package <> "/src/"
      case simplifile.is_directory(src_dir) {
        Ok(True) -> include_sources(sources, src_dir)
        _ -> sources
      }
    })

  let sources =
    sources
    |> include_sources("./stdlib/")
    |> include_sources("./" <> target_path)

  // process the prelude
  let assert Ok(prelude) = read_source(sources, "gleam")
  let assert Ok(prelude) = glance.module(prelude)
  let typed = typed_ast.new_context()
  let typed = typed_ast.infer_module(typed, prelude, "gleam")

  // parse and typecheck input (recursively)
  let #(typed, _done) = infer_file(sources, typed, ["gleam"], module_id)

  let core = core.lower_context(typed)

  // generate header files for ffi
  headers.module_headers(core)
  |> list.each(fn(item) {
    let #(module, header) = item
    let filepath = case dict.get(sources, module <> ".polyfill") {
      Ok(path) -> path
      _ -> {
        let assert Ok(filepath) = dict.get(sources, module)
        filepath
      }
    }
    let filepath =
      filepath
      |> string.replace(".polyfill.gleam", ".h")
      |> string.replace(".gleam", ".h")
    case header {
      "" -> Nil
      _ -> {
        io.println("Generating " <> filepath)
        let assert Ok(_) = simplifile.write(filepath, header)
        Nil
      }
    }
  })

  // compile to c
  let c_file = target_path <> module_id <> ".c"
  io.println("Generating ./" <> c_file)

  let #(mono, main_name) = mono.run(core, module_id <> "_" <> "main")
  let cc = closure.cc_module(mono)
  let code = codegen.compile_module(cc)

  let template =
    "#include <builtin.h>

$code

int main(int argc, char **argv) {
  init(argc, argv);
  $main
  return 0;
}
"

  let output =
    template
    |> string.replace("$main", main_name <> "();")
    |> string.replace("$code", code)

  case simplifile.write(c_file, output) {
    Ok(_) -> Nil
    _ -> panic as { "Failed to write file " <> c_file }
  }

  // find c files that need to be added to the compilation
  let external_c_files =
    mono.used_modules
    |> set.delete("")
    |> set.to_list()
    |> list.filter_map(fn(module) {
      let filepath = case dict.get(sources, module <> ".polyfill") {
        Ok(path) -> path
        _ -> {
          let assert Ok(filepath) = dict.get(sources, module)
          filepath
        }
      }
      let filepath =
        filepath
        |> string.replace(".polyfill.gleam", ".c")
        |> string.replace(".gleam", ".c")

      case simplifile.is_file(filepath) {
        Ok(True) -> Ok(filepath)
        _ -> Error(Nil)
      }
    })

  // compile the c file to binary
  let binary_name = target_path <> module_id <> ".exe"
  let args = [
    "-lm",
    "-Isrc",
    "-o",
    binary_name,
    c_file,
    "./src/builtin.c",
    ..external_c_files
  ]

  let args = case gc {
    True -> ["-DGC", "-lgc", ..args]
    False -> args
  }

  let args = case release {
    True -> ["-O3", ..args]
    False -> args
  }

  let args = case debug {
    True -> ["-g3", ..args]
    False -> args
  }

  io.println(string.join([compiler, ..args], " "))

  io.println("Generating ./" <> binary_name)
  case shellout.command(compiler, args, ".", []) {
    Ok(_) -> Nil
    Error(message) -> panic as { "Failed to generate binary:\n" <> message.1 }
  }

  binary_name
}

fn offset_to_line_col(text: String, offset: Int) {
  let memes = string.to_graphemes(text)
  let lines =
    memes
    |> list.take(offset)
    |> string.concat()
    |> string.split("\n")

  let assert Ok(last_line) = list.last(lines)
  let line = list.length(lines)
  let pos = string.length(last_line)
  int.to_string(line)
  <> ":"
  <> int.to_string(pos)
  <> "\n\n"
  <> string.to_graphemes(text)
  |> list.drop(offset - 50)
  |> list.take(200)
  |> string.concat()
}

fn infer_file(
  sources: dict.Dict(String, String),
  c: typed_ast.Context,
  done: List(String),
  module_id: String,
) -> #(typed_ast.Context, List(String)) {
  // add module to "done" list
  let done = [module_id, ..done]
  let assert Ok(source) = dict.get(sources, module_id)

  io.println("Parse " <> source)

  let module =
    read_source(sources, module_id)
    |> result.try(parse_module(module_id, _))
  let module = case module {
    Ok(module) -> module
    Error(error) -> panic as error
  }

  let polyfill_module_id = module_id <> ".polyfill"
  let polyfill_module =
    read_source(sources, polyfill_module_id)
    |> result.try(parse_module(polyfill_module_id, _))

  let module = case polyfill_module {
    Ok(polyfill) -> polyfill.apply(module, polyfill)
    Error(_) -> module
  }

  // infer imports
  let #(c, done) =
    list.fold(module.imports, #(c, done), fn(acc, i) {
      let #(c, done) = acc
      let module_id = i.definition.module
      case list.contains(done, module_id) {
        True -> #(c, done)
        False -> infer_file(sources, c, done, module_id)
      }
    })

  // infer this file
  io.println("Check " <> source)
  let c = typed_ast.infer_module(c, module, module_id)

  #(c, done)
}

fn parse_module(file: String, input: String) {
  case glance.module(input) {
    Ok(mod) -> Ok(mod)
    Error(e) ->
      case e {
        glance.UnexpectedEndOfInput ->
          Error("Failed to parse " <> file <> ". Unexpected end of input.")
        glance.UnexpectedToken(t, p) ->
          Error(
            "Failed to parse "
            <> file
            <> ". Unexpected token "
            <> string.inspect(t)
            <> "\n\nat "
            <> file
            <> ":"
            <> offset_to_line_col(input, p.byte_offset),
          )
      }
  }
}
