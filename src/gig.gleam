import compiler
import shellout

pub fn main() {
  let assert [file_name] = shellout.arguments()
  compiler.compile(file_name)
}
