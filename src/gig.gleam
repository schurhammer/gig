import argv
import gig/compiler
import gleam/list
import gleam/result
import gleam/string

pub fn main() {
  let args = argv.load().arguments

  let #(flags, files) =
    list.partition(args, fn(a) { string.starts_with(a, "--") })

  let compiler =
    list.find_map(flags, fn(flag) {
      case flag {
        "--compiler=" <> compiler -> Ok(compiler)
        _ -> Error(Nil)
      }
    })
    |> result.unwrap("clang")

  let gc = list.contains(flags, "--gc")
  let release = list.contains(flags, "--release")
  let debug = list.contains(flags, "--debug")

  case files {
    [file, ..] -> {
      compiler.compile(file, compiler:, gc:, release:, debug:)
    }
    _ -> panic as "no file provided"
  }
}
