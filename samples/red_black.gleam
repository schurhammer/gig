//// 1

pub fn main() {
  let t = new(compare)
  let t = insert(t, 1, 1 * 3)
  let t = insert(t, 2, 2 * 3)
  let t = insert(t, 3, 3 * 3)
  let t = insert(t, 4, 4 * 3)

  let t = delete(t, 3)

  let r1 = case find(t, 2) {
    Ok(x) -> x == 2 * 3
    _ -> False
  }

  let r2 = case find(t, 3) {
    Error(_) -> True
    _ -> False
  }

  let x = fold(t, 0, fn(a, k, v) { a + v })
  let r3 = x == 1 * 3 + 2 * 3 + 4 * 3

  let r = case r1 && r2 && r3 {
    True -> 1
    False -> 2
  }
  echo r
}

fn compare(a: Int, with b: Int) -> Order {
  case a == b {
    True -> Eq
    False ->
      case a < b {
        True -> Lt
        False -> Gt
      }
  }
}

pub type Order {
  Eq
  Lt
  Gt
}

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

pub opaque type Map(k, v) {
  Map(root: Node(k, v), compare: fn(k, k) -> Order)
}

/// Creates a new empty map with the provided comparison function for keys.
pub fn new(compare: fn(k, k) -> Order) -> Map(k, v) {
  Map(E, compare)
}

/// Removes all elements from the map, resulting in an empty map.
/// Time complexity: O(1)
pub fn clear(tree: Map(k, v)) -> Map(k, v) {
  Map(E, tree.compare)
}

// TODO is this O(1) amortised?
/// Inserts a new key-value pair into the map.
/// If the key already exists, its associated value is updated with the new value.
/// Time complexity: O(log n)
pub fn insert(tree: Map(k, v), key: k, value: v) -> Map(k, v) {
  Map(blacken(ins(tree.root, #(key, value), tree.compare)), tree.compare)
}

// TODO is this O(1) amortised?
/// Removes a key-value pair from the map, if the key exists.
/// Time complexity: O(log n)
pub fn delete(tree: Map(k, v), key: k) -> Map(k, v) {
  Map(del(redden(tree.root), key, tree.compare), tree.compare)
}

/// Searches for a key in the map and returns the associated value if found.
/// Time complexity: O(log n)
pub fn find(tree: Map(k, v), key: k) -> Result(v, Nil) {
  case do_find(tree.root, key, tree.compare) {
    Ok(entry) -> Ok(entry.1)
    _ -> Error(Nil)
  }
}

/// Applies a function to every key-value pair in the map, accumulating
/// the results with the provided initial accumulator value.
/// Time complexity: O(n)
pub fn fold(tree: Map(k, v), acc: b, fun: fn(b, k, v) -> b) -> b {
  do_fold(tree.root, acc, fun)
}

/// Applies a function to every key-value pair in the map, accumulating
/// the results with the provided initial accumulator value, but in reverse order.
/// Time complexity: O(n)
pub fn foldr(tree: Map(k, v), acc: b, fun: fn(b, k, v) -> b) -> b {
  do_foldr(tree.root, acc, fun)
}

fn ins(node: Node(k, v), x: #(k, v), compare: fn(k, k) -> Order) -> Node(k, v) {
  case node {
    E -> T(R, E, x, E)
    T(c, k, y, b) ->
      case compare(x.0, y.0) {
        Lt -> balance(c, ins(k, x, compare), y, b)
        Gt -> balance(c, k, y, ins(b, x, compare))
        Eq -> T(c, k, x, b)
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

fn del(node: Node(k, v), x: k, compare: fn(k, k) -> Order) -> Node(k, v) {
  case node {
    E -> node
    T(R, E, y, E) ->
      case compare(x, y.0) {
        Eq -> E
        _ -> node
      }
    T(B, E, y, E) ->
      case compare(x, y.0) {
        Eq -> EE
        _ -> node
      }
    T(B, T(R, E, y, E) as l, z, E) ->
      case compare(x, z.0) {
        Lt -> T(B, del(l, x, compare), z, E)
        Gt -> node
        Eq -> T(B, E, y, E)
      }
    T(c, k, y, b) ->
      case compare(x, y.0) {
        Lt -> rotate(c, del(k, x, compare), y, b)
        Gt -> rotate(c, k, y, del(b, x, compare))
        Eq ->
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

fn do_find(
  node: Node(k, v),
  key: k,
  compare: fn(k, k) -> Order,
) -> Result(#(k, v), Nil) {
  case node {
    T(_, l, k, r) ->
      case compare(key, k.0) {
        Lt -> do_find(l, key, compare)
        Gt -> do_find(r, key, compare)
        Eq -> Ok(k)
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
