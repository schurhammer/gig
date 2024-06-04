import argv
import compiler
import glint

fn nogc_flag() -> glint.Flag(Bool) {
  glint.bool_flag("nogc")
  |> glint.flag_default(False)
  |> glint.flag_help("Disable GC")
}

fn release_flag() -> glint.Flag(Bool) {
  glint.bool_flag("release")
  |> glint.flag_default(False)
  |> glint.flag_help("Enables optimisations.")
}

fn compile() -> glint.Command(Nil) {
  use <- glint.command_help("Compiles the file!")
  use nogc <- glint.flag(nogc_flag())
  use release <- glint.flag(release_flag())
  use file <- glint.named_arg("file")
  use named, _args, flags <- glint.command()
  let file = file(named)
  let nogc = case nogc(flags) {
    Ok(flag) -> flag
    _ -> False
  }
  let release = case release(flags) {
    Ok(flag) -> flag
    _ -> False
  }
  compiler.compile(file, !nogc, release)
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
