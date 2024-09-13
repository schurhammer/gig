import gig/compiler

import argv
import glint

fn gc_flag() -> glint.Flag(Bool) {
  glint.bool_flag("gc")
  |> glint.flag_default(False)
  |> glint.flag_help("Disable GC")
}

fn release_flag() -> glint.Flag(Bool) {
  glint.bool_flag("release")
  |> glint.flag_default(False)
  |> glint.flag_help("Enables optimisations.")
}

fn compiler_flag() -> glint.Flag(String) {
  glint.string_flag("compiler")
  |> glint.flag_default("clang")
  |> glint.flag_help("Which c compiler.")
}

fn compile() -> glint.Command(Nil) {
  use <- glint.command_help("Compiles the file!")
  use gc <- glint.flag(gc_flag())
  use release <- glint.flag(release_flag())
  use compiler <- glint.flag(compiler_flag())
  use file <- glint.named_arg("file")
  use named, _args, flags <- glint.command()
  let file = file(named)
  let gc = case gc(flags) {
    Ok(flag) -> flag
    _ -> False
  }
  let release = case release(flags) {
    Ok(flag) -> flag
    _ -> False
  }
  let compiler = case compiler(flags) {
    Ok(flag) -> flag
    Error(_) -> "clang"
  }
  compiler.compile(file, compiler, gc, release)
  Nil
}

pub fn main() {
  let args = argv.load().arguments
  // create a new glint instance
  glint.new()
  |> glint.with_name("gig")
  // with pretty help enabled, using the built-in colours
  |> glint.pretty_help(glint.default_pretty_help())
  // with a root command that executes the `hello` function
  |> glint.add(at: [], do: compile())
  // execute given arguments from stdin
  |> glint.run(args)
}
