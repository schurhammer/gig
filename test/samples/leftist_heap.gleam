//// 21

pub fn main() {
  let h = new(compare)
  let h = insert(h, 3)
  let h = insert(h, 7)
  let h = insert(h, 2)
  let h = insert(h, 6)

  let assert Ok(#(x, h)) = delete_min(h)
  let assert Ok(#(y, h)) = delete_min(h)
  let assert Ok(z) = find_min(h)
  case x == 2 && y == 3 && z == 6 {
    True -> 21
    False -> 22
  }
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

type Tree(a) {
  Empty
  Tree(Int, a, Tree(a), Tree(a))
}

pub opaque type Heap(a) {
  Heap(root: Tree(a), compare: fn(a, a) -> Order)
}

/// Creates a new empty heap with the provided comparison function.
pub fn new(compare: fn(a, a) -> Order) -> Heap(a) {
  Heap(Empty, compare)
}

/// Inserts a new item into the heap, preserving the heap property.
/// Time complexity: O(log n)
pub fn insert(heap: Heap(a), item: a) -> Heap(a) {
  Heap(
    merge_trees(Tree(1, item, Empty, Empty), heap.root, heap.compare),
    heap.compare,
  )
}

/// Returns the minimum element in the heap, if the heap is not empty.
/// Time complexity: O(1)
pub fn find_min(heap: Heap(a)) -> Result(a, Nil) {
  case heap.root {
    Tree(_, x, _, _) -> Ok(x)
    Empty -> Error(Nil)
  }
}

/// Removes and returns the minimum element from the heap, along with the
/// new heap after deletion, if the heap is not empty.
/// Time complexity: O(log n)
pub fn delete_min(heap: Heap(a)) -> Result(#(a, Heap(a)), Nil) {
  case heap.root {
    Tree(_, x, a, b) ->
      Ok(#(x, Heap(merge_trees(a, b, heap.compare), heap.compare)))
    Empty -> Error(Nil)
  }
}

/// Merges two heaps into a new heap containing all elements from both heaps,
/// preserving the heap property.
/// The given heaps must have the same comparison function.
/// Time complexity: O(log n)
pub fn merge(heap1: Heap(a), heap2: Heap(a)) -> Heap(a) {
  let compare = heap1.compare
  Heap(merge_trees(heap1.root, heap2.root, compare), compare)
}

fn merge_trees(h1: Tree(a), h2: Tree(a), compare: fn(a, a) -> Order) -> Tree(a) {
  case h1, h2 {
    h, Empty -> h
    Empty, h -> h
    Tree(_, x, a1, b1), Tree(_, y, a2, b2) ->
      case compare(x, y) {
        Gt -> make(y, a2, merge_trees(h1, b2, compare))
        _ -> make(x, a1, merge_trees(b1, h2, compare))
      }
  }
}

fn make(x, a, b) {
  let rank_a = case a {
    Tree(r, _, _, _) -> r
    Empty -> 0
  }
  let rank_b = case b {
    Tree(r, _, _, _) -> r
    Empty -> 0
  }
  case rank_a < rank_b {
    True -> Tree(rank_a + 1, x, b, a)
    _ -> Tree(rank_b + 1, x, a, b)
  }
}
