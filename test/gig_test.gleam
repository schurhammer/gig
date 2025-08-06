import gig/compiler

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
  let assert Ok(files) = simplifile.get_files("samples")

  // remove old binaries so they don't interfere
  files
  |> list.filter(fn(file) { !string.ends_with(file, ".gleam") })
  |> list.map(fn(file) { simplifile.delete(file) })

  let tests =
    files
    |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
    |> list.filter(fn(file) { !string.ends_with(file, ".polyfill.gleam") })
    |> list.sort(string.compare)
    |> list.map(fn(file) { sample_test(file) })
  describe("samples", tests)
}

fn expected_output(file: String) {
  let assert Ok(content) = simplifile.read(file)

  string.split(content, "\n")
  |> list.filter_map(fn(x) {
    case string.starts_with(x, "//// ") {
      True -> Ok(string.drop_start(x, 5))
      False -> Error(Nil)
    }
  })
  |> string.join("\n")
}

fn sample_test(file) {
  describe(string.replace(file, "./test/samples/", ""), [
    it("self compiled", fn() {
      let assert Ok(_) =
        shellout.command("src/gig.exe", ["--gc", "--release", file], ".", [])

      let binary = string.replace(file, ".gleam", ".exe")
      let expected_output = expected_output(file)
      let assert Ok(output) = shellout.command(binary, [], ".", [])

      expect.to_equal(string.trim(output), string.trim(expected_output))
    }),
    it("without gc", fn() {
      let binary =
        compiler.compile(
          file,
          compiler: "clang",
          gc: False,
          release: True,
          debug: False,
        )
      let expected_output = expected_output(file)
      let assert Ok(output) = shellout.command(binary, [], ".", [])

      expect.to_equal(string.trim(output), string.trim(expected_output))
    }),
    it("with gc", fn() {
      let binary =
        compiler.compile(
          file,
          compiler: "clang",
          gc: True,
          release: True,
          debug: False,
        )
      let expected_output = expected_output(file)
      let assert Ok(output) = shellout.command(binary, [], ".", [])

      expect.to_equal(string.trim(output), string.trim(expected_output))
    }),
    xit("passes valgrind", fn() {
      let binary =
        compiler.compile(
          file,
          compiler: "clang",
          gc: False,
          release: True,
          debug: False,
        )
      let args = ["--error-exitcode=1", binary]
      let output = shellout.command("valgrind", args, ".", [])
      expect.to_be_ok(output)
      Nil
    }),
  ])
}
