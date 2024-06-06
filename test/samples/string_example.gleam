//// hello world

@external(erlang, "_", "_")
pub fn print(x: String) -> String

pub fn println(x: String) {
  print(x)
  print("\n")
  Nil
}

pub fn main() {
  let x = "hello"
  let y = "world"
  println(x <> " " <> y)
}
