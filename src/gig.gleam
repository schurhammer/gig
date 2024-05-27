import compile
import shellout

pub fn main() {
  let assert [file_name] = shellout.arguments()
  compile.compile(file_name)
}
