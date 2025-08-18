//// Ok(6)

pub fn main() {
  let items = [1, 2, 3]
  let sum = try_fold(items, 0, fn(acc, i) { Ok(acc + i) })
  echo sum
}

pub fn try_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> Result(acc, e),
) -> Result(acc, e) {
  case list {
    [] -> Ok(initial)
    [first, ..rest] ->
      case fun(initial, first) {
        Ok(result) -> try_fold(rest, result, fun)
        Error(_) as error -> error
      }
  }
}
