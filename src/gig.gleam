import argv
import gig/compiler
import gleam/list
import gleam/string

pub fn main() {
  let args = argv.load().arguments

  let #(flags, files) =
    list.partition(args, fn(a) { string.starts_with(a, "--") })

  let gc = list.contains(flags, "--gc")
  let release = list.contains(flags, "--release")
  let compiler = "clang"

  case files {
    [file, ..] -> {
      compiler.compile(file, compiler, gc, release)
    }
    _ -> panic as "no file provided"
  }
}
