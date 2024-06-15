import gleam/order

pub fn map(list: List(a), f: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [x, ..xs] -> [f(x), ..map(xs, f)]
  }
}

pub fn fold(list: List(a), init: b, f: fn(b, a) -> b) -> b {
  case list {
    [] -> init
    [x, ..xs] -> fold(xs, f(init, x), f)
  }
}

pub fn filter(list: List(a), f: fn(a) -> Bool) -> List(a) {
  case list {
    [] -> []
    [x, ..xs] ->
      case f(x) {
        True -> [x, ..filter(xs, f)]
        False -> filter(xs, f)
      }
  }
}

pub fn take(from: List(a), n: Int) -> List(a) {
  case from {
    [] -> []
    _ if n <= 0 -> []
    [x, ..xs] -> [x, ..take(xs, n - 1)]
  }
}

pub fn drop(from: List(a), n: Int) -> List(a) {
  case from {
    [] -> []
    _ if n <= 0 -> from
    [x, ..xs] -> drop(xs, n - 1)
  }
}

pub fn length(of: List(a)) -> Int {
  case of {
    [] -> 0
    [_, ..xs] -> 1 + length(xs)
  }
}

pub fn sort(list: List(a), by compare: fn(a, a) -> order.Order) -> List(a) {
  do_sort(list, compare, length(list))
}

fn merge_sort(
  a: List(a),
  b: List(a),
  compare: fn(a, a) -> order.Order,
) -> List(a) {
  case a, b {
    [], _ -> b
    _, [] -> a
    [ax, ..ar], [bx, ..br] ->
      case compare(ax, bx) {
        order.Lt -> [ax, ..merge_sort(ar, b, compare)]
        _ -> [bx, ..merge_sort(a, br, compare)]
      }
  }
}

fn do_sort(
  list: List(a),
  compare: fn(a, a) -> order.Order,
  list_length: Int,
) -> List(a) {
  case list_length < 2 {
    True -> list
    False -> {
      let split_length = list_length / 2
      let a_list = take(list, split_length)
      let b_list = drop(list, split_length)
      merge_sort(
        do_sort(a_list, compare, split_length),
        do_sort(b_list, compare, list_length - split_length),
        compare,
      )
    }
  }
}
