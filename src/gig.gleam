import argv
import gig/compiler
import gleam/io

pub fn main() {
  case argv.load().arguments {
    [file, ..flags] -> {
      // TODO flags
      let gc = False
      let release = True
      let compiler = "clang"
      compiler.compile(file, compiler, gc, release)
    }
    _ -> panic as "no file provided"
  }
}
