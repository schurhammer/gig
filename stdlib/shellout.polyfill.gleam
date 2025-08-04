fn do_command(
  executable: String,
  arguments: List(String),
  directory: String,
  options: dict.Dict(CommandOpt, Bool),
  environment: List(#(String, String)),
) -> Result(String, #(Int, String)) {
  let options = dict.to_list(options)
  c_do_command(executable, arguments, directory, options, environment)
}

@external(c, "", "shellout_c_do_command")
fn c_do_command(
  executable: String,
  arguments: List(String),
  directory: String,
  options: List(#(CommandOpt, Bool)),
  environment: List(#(String, String)),
) -> Result(String, #(Int, String))

@external(c, "", "shellout_arguments")
pub fn arguments() -> List(String)

@external(c, "", "shellout_exit")
pub fn exit(status: Int) -> Nil

@external(c, "", "shellout_which")
pub fn which(executable: String) -> Result(String, String)
