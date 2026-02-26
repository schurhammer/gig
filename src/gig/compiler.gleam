import anf/codegen
import gig/closure
import gig/core
import gig/headers
import gig/mono
import gig/patch
import gig/typed_ast
import gleam/bit_array
import gleam/dict
import gleam/set
import listx

import gleam/int
import gleam/result

import glance
import shellout
import simplifile

import gleam/io
import gleam/list
import gleam/string

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

pub type CompileOptions {
  CompileOptions(
    compiler: String,
    gc: Bool,
    release: Bool,
    debug: Bool,
    c_only: Bool,
    headers: Bool,
  )
}

pub fn default_options() -> CompileOptions {
  CompileOptions(
    compiler: "clang",
    gc: False,
    release: False,
    debug: False,
    c_only: False,
    headers: False,
  )
}

// returns the file name of the binary (empty string for headers-only mode)
pub fn compile(gleam_file_name: String, options: CompileOptions) {
  let split = string.split(gleam_file_name, "/")
  let assert Ok(target_file) = list.last(split)
  let module_id = string.replace(target_file, ".gleam", "")

  let target_path =
    listx.delete_last(split)
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
    |> include_sources("./patch/")
    |> include_sources("./" <> target_path)

  // process the prelude
  let assert Ok(source_text) = read_source(sources, "gleam")
  let assert Ok(prelude) = glance.module(source_text)
  let typed = typed_ast.new_context()
  let assert Ok(typed) =
    typed_ast.infer_module(typed, prelude, "gleam", source_text)

  // parse and typecheck input (recursively)
  let #(typed, _done) = infer_file(sources, typed, ["gleam"], module_id)

  let core = core.lower_context(typed)

  case options.headers {
    True -> generate_headers_only(core, sources)
    False -> compile_to_binary(core, sources, module_id, target_path, options)
  }
}

fn generate_headers_only(
  core: core.Context,
  sources: dict.Dict(String, String),
) -> String {
  headers.module_headers(core)
  |> list.each(fn(item) {
    let #(module, header) = item
    // prefer the patch file location for placing the header
    let filepath = case dict.get(sources, module <> ".patch") {
      Ok(path) -> string.replace(path, ".patch.gleam", ".h")
      _ -> {
        let assert Ok(path) = dict.get(sources, module)
        string.replace(path, ".gleam", ".h")
      }
    }
    case header {
      "" -> Nil
      _ -> {
        io.println("Generating " <> filepath)
        let assert Ok(_) = simplifile.write(filepath, header)
        Nil
      }
    }
  })
  ""
}

fn compile_to_binary(
  core: core.Context,
  sources: dict.Dict(String, String),
  module_id: String,
  target_path: String,
  options: CompileOptions,
) -> String {
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
      let filepath = case dict.get(sources, module <> ".patch") {
        Ok(path) -> path
        _ -> {
          let assert Ok(filepath) = dict.get(sources, module)
          filepath
        }
      }
      let filepath =
        filepath
        |> string.replace(".patch.gleam", ".c")
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
    "-Ipatch",
    "-o",
    binary_name,
    c_file,
    "patch/builtin.c",
    ..external_c_files
  ]

  let args = case options.gc {
    True -> ["-DGC", "-lgc", ..args]
    False -> args
  }

  let args = case options.release {
    True -> ["-O3", "-flto", ..args]
    False -> args
  }

  let args = case options.debug {
    True -> ["-g3", ..args]
    False -> args
  }

  io.println(string.join([options.compiler, ..args], " "))
  case options.c_only {
    False -> {
      io.println("Generating ./" <> binary_name)
      let result = shellout.command(options.compiler, args, ".", [])
      case result {
        Ok(_) -> Nil
        Error(message) ->
          panic as { "Failed to generate binary:\n" <> message.1 }
      }
    }
    True -> Nil
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
  let assert Ok(source_text) = read_source(sources, module_id)
  let assert Ok(module) = parse_module(module_id, source_text)

  let patch_module_id = module_id <> ".patch"
  let patch_module =
    read_source(sources, patch_module_id)
    |> result.try(parse_module(patch_module_id, _))

  let module = case patch_module {
    Ok(patch) -> patch.apply(module, patch)
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
  case typed_ast.infer_module(c, module, module_id, source_text) {
    Ok(c) -> #(c, done)
    Error(err) ->
      panic as error(source, err.location, typed_ast.inspect_error(err))
  }
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

fn error(
  source: String,
  location: typed_ast.Location,
  message: String,
) -> String {
  let start = location.span.start
  let end = location.span.end

  let context_before = case start {
    start if start < 100 -> string.slice(source, 0, start)
    start -> string.slice(source, start - 100, 100)
  }
  let context_before = string.split(context_before, "\n")
  let context_before =
    context_before
    |> list.drop(list.length(context_before) - 3)
    |> list.drop_while(fn(x) { x == "" })
    |> string.join("\n")

  let context_after =
    source
    |> string.slice(end, 100)
    |> string.split("\n")
    |> list.take(3)
    |> string.join("\n")

  let error_part = string.slice(source, start, end - start)

  // TODO this unicode escape sequence is not valid in c, needs compilation
  // let escape = "\u{001b}["
  let assert Ok(escape) = bit_array.to_string(<<27, 91>>)
  let clear = escape <> "0m"
  let red_underline = escape <> "4;31m"

  "Error: "
  <> message
  <> "\n\nat: "
  <> location.module
  <> "."
  <> location.definition
  <> "\n\n"
  <> context_before
  <> red_underline
  <> error_part
  <> clear
  <> context_after
}
