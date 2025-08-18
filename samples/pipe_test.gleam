//// 5
//// 100

pub fn main() {
  10
  |> sub(3)
  |> sub(2)
  |> echo

  100
  |> fn(x) { x }
  |> echo
}

fn sub(a, b) {
  a - b
}
