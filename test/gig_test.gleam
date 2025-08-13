import gig/compiler
import gleam/io
import startest/assertion_error

import gleam/list
import gleam/string

import shellout
import simplifile
import startest.{describe, it, xit}
import startest/expect

pub fn main() {
  // self-compile if binary doesn't exist
  case simplifile.is_file("src/gig.exe") {
    Ok(True) -> Nil
    _ -> {
      io.println_error("gig.exe does not exist, compiling now ..")
      compiler.compile(
        "src/gig",
        compiler: "clang",
        gc: True,
        release: True,
        debug: False,
      )
      Nil
    }
  }

  // run the tests
  startest.run(startest.default_config())
}

pub fn expect_equal_string(actual: String, expected: String) -> Nil {
  let diff =
    shellout.command(
      run: "bash",
      with: [
        "-c",
        "diff -u --color=always --label Actual --label Expected <(printf '%s' \"$A\") <(printf '%s' \"$B\")",
      ],
      in: ".",
      opt: [shellout.SetEnvironment([#("A", actual), #("B", expected)])],
    )

  case diff {
    Ok(_) -> Nil
    Error(#(_, diff)) ->
      assertion_error.AssertionError(diff, "", "")
      |> assertion_error.raise
  }
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
    it("native compiled", fn() {
      let assert Ok(_) =
        shellout.command("src/gig.exe", ["--gc", "--release", file], ".", [])

      let binary = string.replace(file, ".gleam", ".exe")
      let expected_output = expected_output(file)
      let assert Ok(output) = shellout.command(binary, [], ".", [])

      expect_equal_string(string.trim(output), string.trim(expected_output))
    }),
    it("erlang compiled", fn() {
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

      expect_equal_string(string.trim(output), string.trim(expected_output))
    }),
    it("native/erlang diff", fn() {
      // compile using native binary
      let assert Ok(_) =
        shellout.command("src/gig.exe", ["--gc", "--release", file], ".", [])
      let c_file = string.replace(file, ".gleam", ".c")
      let assert Ok(self_compiled_c) = simplifile.read(c_file)
      let assert Ok(_) = simplifile.write(c_file <> ".bak", self_compiled_c)

      // compile using erlang runtime
      compiler.compile(
        file,
        compiler: "clang",
        gc: True,
        release: True,
        debug: False,
      )
      let assert Ok(erlang_compiled_c) = simplifile.read(c_file)

      expect_equal_string(self_compiled_c, erlang_compiled_c)
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
