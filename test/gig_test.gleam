import compiler
import shellout
import simplifile

import gleeunit
import gleeunit/should

import gleam/io
import gleam/list
import gleam/string

pub fn main() {
  gleeunit.main()
}

pub fn run_all_samples_test() {
  let assert Ok(files) = simplifile.get_files("./test/samples")
  files
  |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
  |> list.each(fn(file) { run_sample(file) })
}

fn run_sample(file) {
  io.print_error("Running ")
  io.println_error(file)

  let binary = compiler.compile(file)

  // parse the expected output out of the file's doc comment
  let assert Ok(content) = simplifile.read(file)
  let expected_output =
    string.split(content, "\n")
    |> list.filter_map(fn(x) {
      case string.starts_with(x, "//// ") {
        True -> Ok(string.drop_left(x, 5))
        False -> Error(Nil)
      }
    })
    |> string.join("\n")

  // run the program
  let assert Ok(output) = shellout.command(binary, [], ".", [])

  string.trim(output) |> should.equal(string.trim(expected_output))
}
