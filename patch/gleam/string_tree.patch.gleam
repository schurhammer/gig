import gleam/list

pub type StringTree {
  Leaf(value: String)
  Node(left: StringTree, right: StringTree)
}

/// Create an empty `StringTree`. Useful as the start of a pipe chaining many
/// trees together.
///
pub fn new() -> StringTree {
  Leaf("")
}

/// Prepends a `String` onto the start of some `StringTree`.
///
/// Runs in constant time.
///
pub fn prepend(to tree: StringTree, prefix prefix: String) -> StringTree {
  append_tree(from_string(prefix), tree)
}

/// Appends a `String` onto the end of some `StringTree`.
///
/// Runs in constant time.
///
pub fn append(to tree: StringTree, suffix second: String) -> StringTree {
  append_tree(tree, from_string(second))
}

/// Prepends some `StringTree` onto the start of another.
///
/// Runs in constant time.
///
pub fn prepend_tree(
  to tree: StringTree,
  prefix prefix: StringTree,
) -> StringTree {
  append_tree(prefix, tree)
}

/// Appends some `StringTree` onto the end of another.
///
/// Runs in constant time.
///
pub fn append_tree(to tree: StringTree, suffix suffix: StringTree) -> StringTree {
  Node(tree, suffix)
}

/// Converts a list of strings into a `StringTree`.
///
/// Runs in constant time.
///
pub fn from_strings(strings: List(String)) -> StringTree {
  case strings {
    [] -> new()
    [x] -> Leaf(x)
    [x, ..rest] -> Node(Leaf(x), from_strings(rest))
  }
}

/// Joins a list of trees into a single tree.
///
/// Runs in constant time.
///
pub fn concat(trees: List(StringTree)) -> StringTree {
  case trees {
    [] -> new()
    [x] -> x
    [x, ..rest] -> Node(x, concat(rest))
  }
}

/// Converts a string into a `StringTree`.
///
/// Runs in constant time.
///
pub fn from_string(string: String) -> StringTree {
  Leaf(string)
}

/// Turns a `StringTree` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub fn to_string(tree: StringTree) -> String {
  case tree {
    Leaf("") -> ""
    Leaf(string) -> string
    Node(left, right) -> to_string(left) <> to_string(right)
  }
}

/// Returns the size of the `StringTree` in bytes.
///
pub fn byte_size(tree: StringTree) -> Int {
  case tree {
    Leaf("") -> 0
    Leaf(string) -> length_string(string)
    Node(left, right) -> byte_size(left) + byte_size(right)
  }
}

/// Joins the given trees into a new tree separated with the given string.
///
pub fn join(trees: List(StringTree), with sep: String) -> StringTree {
  trees
  |> list.intersperse(from_string(sep))
  |> concat
}

/// Converts a `StringTree` to a new one where the contents have been
/// lowercased.
///
pub fn lowercase(tree: StringTree) -> StringTree {
  // case tree {
  //   Leaf(string) -> Leaf(string.lowercase(string))
  //   Node(left, right) -> Node(lowercase(left), lowercase(right))
  // }
  todo
}

/// Converts a `StringTree` to a new one where the contents have been
/// uppercased.
///
pub fn uppercase(tree: StringTree) -> StringTree {
  // case tree {
  //   Leaf(string) -> Leaf(string.uppercase(string))
  //   Node(left, right) -> Node(uppercase(left), uppercase(right))
  // }
  todo
}

/// Converts a `StringTree` to a new one with the contents reversed.
///
pub fn reverse(tree: StringTree) -> StringTree {
  tree
  |> to_string
  |> do_to_graphemes
  |> list.reverse
  |> from_strings
}

fn do_to_graphemes(string: String) -> List(String)

@external(c, "", "slice_string")
pub fn slice_string(str: String, offset: Int, length: Int) -> String

@external(c, "", "length_string")
fn length_string(str: String) -> Int

/// Splits a `StringTree` on a given pattern into a list of trees.
///
pub fn split(tree: StringTree, on pattern: String) -> List(StringTree) {
  let string = to_string(tree)
  let pattern_len = length_string(pattern)

  case pattern_len {
    0 -> [tree]
    _ -> do_split(string, pattern, pattern_len, 0, [])
  }
}

fn do_split(string: String, pattern: String, pattern_len: Int, start: Int, acc: List(StringTree)) -> List(StringTree) {
  let string_len = length_string(string)
  case find_pattern(string, pattern, pattern_len, start, string_len) {
    Ok(index) -> {
      let part = slice_string(string, start, index - start)
      let new_acc = [from_string(part), ..acc]
      do_split(string, pattern, pattern_len, index + pattern_len, new_acc)
    }
    Error(Nil) -> {
      let part = slice_string(string, start, string_len - start)
      list.reverse([from_string(part), ..acc])
    }
  }
}

fn find_pattern(string: String, pattern: String, pattern_len: Int, start: Int, string_len: Int) -> Result(Int, Nil) {
  case start + pattern_len > string_len {
    True -> Error(Nil)
    False -> {
      let substr = slice_string(string, start, pattern_len)
      case substr == pattern {
        True -> {
          Ok(start)
        }
        False -> find_pattern(string, pattern, pattern_len, start + 1, string_len)
      }
    }
  }
}

/// Replaces all instances of a pattern with a given string substitute.
///
pub fn replace(
  in tree: StringTree,
  each pattern: String,
  with substitute: String,
) -> StringTree {
  split(tree, pattern)
  |> join(substitute)
}

/// Compares two string trees to determine if they have the same textual
/// content.
///
/// Comparing two string trees using the `==` operator may return `False` even
/// if they have the same content as they may have been build in different ways,
/// so using this function is often preferred.
///
/// ## Examples
///
/// ```gleam
/// from_strings(["a", "b"]) == from_string("ab")
/// // -> False
/// ```
///
/// ```gleam
/// is_equal(from_strings(["a", "b"]), from_string("ab"))
/// // -> True
/// ```
///
pub fn is_equal(a: StringTree, b: StringTree) -> Bool {
  to_string(a) == to_string(b)
}

/// Inspects a `StringTree` to determine if it is equivalent to an empty string.
///
/// ## Examples
///
/// ```gleam
/// from_string("ok") |> is_empty
/// // -> False
/// ```
///
/// ```gleam
/// from_string("") |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// from_strings([]) |> is_empty
/// // -> True
/// ```
///
pub fn is_empty(tree: StringTree) -> Bool {
  case tree {
    Leaf("") -> True
    Leaf(_) -> False
    Node(left, right) -> is_empty(left) && is_empty(right)
  }
}
