import gleam/order

type Color {
  R
  B
  BB
}

type Node(k, v) {
  E
  EE
  // TODO try flattening the k, v, pair into the tuple -- benchmark!
  T(c: Color, l: Node(k, v), k: #(k, v), r: Node(k, v))
}

pub opaque type Dict(k, v) {
  Dict(root: Node(k, v))
}

/// Creates a new empty map with the provided comparison function for keys.
pub fn new() -> Dict(k, v) {
  Dict(E)
}

pub fn to_list(dict: Dict(k, v)) -> List(#(k, v)) {
  foldr(dict, [], fn(l, k, v) { [#(k, v), ..l] })
}

/// Removes all elements from the map, resulting in an empty map.
/// Time complexity: O(1)
pub fn clear(tree: Dict(k, v)) -> Dict(k, v) {
  Dict(E)
}

// TODO is this O(1) amortised?
/// Inserts a new key-value pair into the map.
/// If the key already exists, its associated value is updated with the new value.
/// Time complexity: O(log n)
pub fn insert(tree: Dict(k, v), key: k, value: v) -> Dict(k, v) {
  Dict(blacken(ins(tree.root, #(key, value))))
}

// TODO is this O(1) amortised?
/// Removes a key-value pair from the map, if the key exists.
/// Time complexity: O(log n)
pub fn delete(tree: Dict(k, v), key: k) -> Dict(k, v) {
  Dict(del(redden(tree.root), key))
}

/// Searches for a key in the map and returns the associated value if found.
/// Time complexity: O(log n)
pub fn get(tree: Dict(k, v), key: k) -> Result(v, Nil) {
  case do_find(tree.root, key) {
    Ok(entry) -> Ok(entry.1)
    _ -> Error(Nil)
  }
}

/// Applies a function to every key-value pair in the map, accumulating
/// the results with the provided initial accumulator value.
/// Time complexity: O(n)
pub fn fold(
  over dict: Dict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  do_fold(dict.root, initial, fun)
}

fn foldr(tree: Dict(k, v), acc: b, fun: fn(b, k, v) -> b) -> b {
  do_foldr(tree.root, acc, fun)
}

fn ins(node: Node(k, v), x: #(k, v)) -> Node(k, v) {
  case node {
    E -> T(R, E, x, E)
    T(c, k, y, b) ->
      case compare(x.0, y.0) {
        order.Lt -> balance(c, ins(k, x), y, b)
        order.Gt -> balance(c, k, y, ins(b, x))
        order.Eq -> T(c, k, x, b)
      }
    _ -> node
  }
}

fn blacken(node: Node(k, v)) -> Node(k, v) {
  case node {
    T(R, T(R, _, _, _) as l, y, c) -> T(B, l, y, c)
    T(R, k, x, T(R, _, _, _) as r) -> T(B, k, x, r)
    t -> t
  }
}

fn balance(c: Color, l: Node(k, v), v: #(k, v), r: Node(k, v)) -> Node(k, v) {
  case c, l, v, r {
    B, T(R, T(R, k, x, b), y, c), z, d -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, T(R, k, x, T(R, b, y, c)), z, d -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, k, x, T(R, T(R, b, y, c), z, d) -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    B, k, x, T(R, b, y, T(R, c, z, d)) -> T(R, T(B, k, x, b), y, T(B, c, z, d))
    BB, k, x, T(R, T(R, b, y, c), z, d) -> T(B, T(B, k, x, b), y, T(B, c, z, d))
    BB, T(R, k, x, T(R, b, y, c)), z, d -> T(B, T(B, k, x, b), y, T(B, c, z, d))
    c, k, x, b -> T(c, k, x, b)
  }
}

fn redden(node: Node(k, v)) -> Node(k, v) {
  case node {
    T(B, T(B, _, _, _) as l, y, T(B, _, _, _) as r) -> T(R, l, y, r)
    t -> t
  }
}

fn del(node: Node(k, v), x: k) -> Node(k, v) {
  case node {
    E -> node
    T(R, E, y, E) ->
      case compare(x, y.0) {
        order.Eq -> E
        _ -> node
      }
    T(B, E, y, E) ->
      case compare(x, y.0) {
        order.Eq -> EE
        _ -> node
      }
    T(B, T(R, E, y, E) as l, z, E) ->
      case compare(x, z.0) {
        order.Lt -> T(B, del(l, x), z, E)
        order.Gt -> node
        order.Eq -> T(B, E, y, E)
      }
    T(c, k, y, b) ->
      case compare(x, y.0) {
        order.Lt -> rotate(c, del(k, x), y, b)
        order.Gt -> rotate(c, k, y, del(b, x))
        order.Eq ->
          case min_del(b) {
            Min(y1, b1) -> rotate(c, k, y1, b1)
            None -> E
          }
      }
    _ -> node
  }
}

fn rotate(c: Color, l: Node(k, v), v: #(k, v), r: Node(k, v)) -> Node(k, v) {
  case c, l, v, r {
    R, T(BB, k, x, b), y, T(B, c, z, d) ->
      balance(B, T(R, T(B, k, x, b), y, c), z, d)
    R, EE, y, T(B, c, z, d) -> balance(B, T(R, E, y, c), z, d)
    R, T(B, k, x, b), y, T(BB, c, z, d) ->
      balance(B, k, x, T(R, b, y, T(B, c, z, d)))
    R, T(B, k, x, b), y, EE -> balance(B, k, x, T(R, b, y, E))
    B, T(BB, k, x, b), y, T(B, c, z, d) ->
      balance(BB, T(R, T(B, k, x, b), y, c), z, d)
    B, EE, y, T(B, c, z, d) -> balance(BB, T(R, E, y, c), z, d)
    B, T(B, k, x, b), y, T(BB, c, z, d) ->
      balance(BB, k, x, T(R, b, y, T(B, c, z, d)))
    B, T(B, k, x, b), y, EE -> balance(BB, k, x, T(R, b, y, E))
    B, T(BB, k, w, b), x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, T(B, k, w, b), x, c), y, d), z, e)
    B, EE, x, T(R, T(B, c, y, d), z, e) ->
      T(B, balance(B, T(R, E, x, c), y, d), z, e)
    B, T(R, k, w, T(B, b, x, c)), y, T(BB, d, z, e) ->
      T(B, k, w, balance(B, b, x, T(R, c, y, T(B, d, z, e))))
    B, T(R, k, w, T(B, b, x, c)), y, EE ->
      T(B, k, w, balance(B, b, x, T(R, c, y, E)))
    c, k, x, b -> T(c, k, x, b)
  }
}

type MinDel(k, v) {
  Min(#(k, v), Node(k, v))
  None
}

fn min_del(node: Node(k, v)) -> MinDel(k, v) {
  case node {
    T(R, E, x, E) -> Min(x, E)
    T(B, E, x, E) -> Min(x, EE)
    T(B, E, x, T(R, E, y, E)) -> Min(x, T(B, E, y, E))
    T(c, k, x, b) ->
      case min_del(k) {
        Min(x1, a1) -> Min(x1, rotate(c, a1, x, b))
        None -> None
      }
    _ -> None
  }
}

fn do_find(node: Node(k, v), key: k) -> Result(#(k, v), Nil) {
  case node {
    T(_, l, k, r) ->
      case compare(key, k.0) {
        order.Lt -> do_find(l, key)
        order.Gt -> do_find(r, key)
        order.Eq -> Ok(k)
      }
    _ -> Error(Nil)
  }
}

fn do_fold(node: Node(k, v), acc: a, fun: fn(a, k, v) -> a) -> a {
  case node {
    T(_, r, v, l) -> {
      let acc = do_fold(r, acc, fun)
      let acc = fun(acc, v.0, v.1)
      let acc = do_fold(l, acc, fun)
      acc
    }
    _ -> acc
  }
}

fn do_foldr(node: Node(k, v), acc: a, fun: fn(a, k, v) -> a) -> a {
  case node {
    T(_, r, v, l) -> {
      let acc = do_foldr(l, acc, fun)
      let acc = fun(acc, v.0, v.1)
      let acc = do_foldr(r, acc, fun)
      acc
    }
    _ -> acc
  }
}

fn compare(a: a, b: a) -> order.Order {
  todo
}
