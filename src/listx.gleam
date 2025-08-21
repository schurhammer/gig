import gleam/list

pub fn sane_range(n) {
  list.reverse(do_sane_range(n))
}

fn do_sane_range(n) {
  case n {
    0 -> []
    _ -> [n, ..sane_range(n - 1)]
  }
}

pub fn pop(
  in list: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(#(a, List(a)), Nil) {
  pop_loop(list, is_desired, [])
}

pub fn delete_last(l: List(a)) -> List(a) {
  case l {
    [] | [_] -> []
    [x, ..xs] -> [x, ..delete_last(xs)]
  }
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
