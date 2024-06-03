import compiler
import gleam/list
import gleam/string
import shellout
import simplifile
import startest.{describe, it, xit}
import startest/expect

pub fn main() {
  startest.run(startest.default_config())
}

pub fn all_samples_tests() {
  let assert Ok(files) = simplifile.get_files("./test/samples")

  // remove old binaries so they don't interfere
  files
  |> list.filter(fn(file) { !string.ends_with(file, ".gleam") })
  |> list.map(fn(file) { simplifile.delete(file) })

  let tests =
    files
    |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
    |> list.map(fn(file) { sample_test(file) })
  describe("samples", tests)
}

import gleam/io

fn sample_test(file) {
  describe(string.replace(file, "./test/samples/", ""), [
    it("correct output", fn() {
      io.debug(file)
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

      expect.to_equal(string.trim(output), string.trim(expected_output))
    }),
    xit("passes valgrind", fn() {
      let file = string.replace(file, ".gleam", "")
      let args = ["--error-exitcode=1", file]
      let output = shellout.command("valgrind", args, ".", [])
      expect.to_be_ok(output)
      Nil
    }),
  ])
}
