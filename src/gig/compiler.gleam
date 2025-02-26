import gig/closure
import gig/codegen
import gig/core
import gig/mono
import gig/polyfill
import gig/typed_ast

import gleam/dict
import gleam/int
import gleam/result
import pprint

import glance
import shellout
import simplifile

import gleam/io
import gleam/list
import gleam/string

const build_dir = "./build/c"

const build_src_dir = "./build/c/src"

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

fn include_sources(src_dir: String) {
  // TODO instead of copying files we could create an index of where to find them

  io.println("Sources " <> src_dir)
  let assert Ok(files) = simplifile.get_files(src_dir)

  files
  |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
  |> list.each(fn(src) {
    let dst_file = build_src_dir <> string.replace(src, src_dir, "")
    let dst_path = all_but_last(string.split(dst_file, "/")) |> string.join("/")

    let assert Ok(_) = simplifile.create_directory_all(dst_path)
    let assert Ok(_) = simplifile.copy_file(src, dst_file)
  })
}

fn read_source_file(path) {
  case simplifile.read(build_src_dir <> "/" <> path) {
    Ok(content) -> Ok(content)
    Error(_) -> {
      Error("Failed to read module " <> path)
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
  let assert Ok(target_file) = list.last(split)
  let module_id = string.replace(target_file, ".gleam", "")

  let target_path =
    all_but_last(split)
    |> string.join("/")

  let target_path = case target_path {
    "" -> "."
    _ -> target_path
  }

  // clear build dir
  let _ = simplifile.delete(build_dir)

  // copy dependencies src into build dir
  let packages_dir = "./build/packages"
  let assert Ok(packages) = simplifile.read_directory(packages_dir)
  list.each(packages, fn(package) {
    let src_dir = packages_dir <> "/" <> package <> "/src"
    case simplifile.is_directory(src_dir) {
      Ok(True) -> include_sources(src_dir)
      _ -> Nil
    }
  })

  // let assert Ok(packages) = simplifile.get_files(packages_dir)
  // packages
  // |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
  // |> list.each(fn(src) {
  //   let relative = src |> string.split("/") |> list.drop(5)
  //   let relative_file = relative |> string.join("/")
  //   let relative_path = all_but_last(relative) |> string.join("/")

  //   let dst_path = build_src_dir <> "/" <> relative_path
  //   let dst_file = build_src_dir <> "/" <> relative_file

  //   let assert Ok(_) = simplifile.create_directory_all(dst_path)
  //   let assert Ok(_) = simplifile.copy_file(src, dst_file)
  // })

  // copy stdlib polyfills into build dir
  include_sources("./stdlib")

  // copy target src into build dir
  include_sources("./" <> target_path)

  // process the prelude
  let prelude = read_file(build_src_dir <> "/gleam.gleam")
  let assert Ok(prelude) = glance.module(prelude)
  let c = typed_ast.new_context()
  let c = typed_ast.infer_module(c, prelude, "gleam")

  // parse and typecheck input (recursively)
  let #(typed, _done) = infer_file(c, ["gleam"], module_id)

  // generate code
  let core = core.lower_context(typed)
  // pprint.debug(core.functions |> dict.get("string_example_main"))
  let #(mono, main_name) = mono.run(core, module_id <> "_" <> "main")
  let cc = closure.cc_module(mono)
  let code = codegen.module(cc)

  // insert the generated code into the template
  let template = read_file("./src/template.c")

  let main_call = main_name <> "();\n"

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
  io.println("Generating ./" <> c_file)
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

  io.println("Generating binary ./" <> file_name)
  case shellout.command(compiler, args, ".", []) {
    Ok(_) -> Nil
    Error(message) ->
      io.println_error("Failed to generate binary:\n" <> message.1)
  }

  file_name
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
  c: typed_ast.Context,
  done: List(String),
  module_id: String,
) -> #(typed_ast.Context, List(String)) {
  let file = module_id <> ".gleam"
  let polyfill_file = module_id <> ".polyfill.gleam"
  io.println("Read  " <> build_src_dir <> "/" <> file)

  // add file to "done" list
  let done = [module_id, ..done]

  // parse file
  let assert Ok(module) =
    read_source_file(file)
    |> result.try(parse_module(file, _))

  let polyfill_module =
    read_source_file(polyfill_file)
    |> result.try(parse_module(polyfill_file, _))

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
        False -> infer_file(c, done, module_id)
      }
    })

  // infer this file
  io.println("Check " <> build_src_dir <> "/" <> file)
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
            <> build_src_dir
            <> "/"
            <> file
            <> ":"
            <> offset_to_line_col(input, p.byte_offset),
          )
      }
  }
}
