import gleam/list

pub fn pop(
  in list: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(#(a, List(a)), Nil) {
  pop_loop(list, is_desired, [])
}

fn pop_loop(haystack, predicate, checked) {
  case haystack {
    [] -> Error(Nil)
    [first, ..rest] ->
      case predicate(first) {
        True -> Ok(#(first, list.append(list.reverse(checked), rest)))
        False -> pop_loop(rest, predicate, [first, ..checked])
      }
  }
}
