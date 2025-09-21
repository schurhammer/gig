import gleam/list
import gleam/option.{type Option, None, Some}

@external(c, "", "slice_string")
pub fn slice_string(str: String, offset: Int, length: Int) -> String

@external(c, "", "length_string")
fn length_string(str: String) -> Int

pub type Splitter {
  Splitter(patterns: List(String))
}

pub fn new(substrings: List(String)) -> Splitter {
  substrings
  |> list.filter(fn(x) { x != "" })
  |> make
}

pub fn split(splitter: Splitter, string: String) -> #(String, String, String) {
  let Splitter(patterns) = splitter
  let string_len = length_string(string)

  case find_first_match(patterns, string, 0, string_len) {
    Some(#(position, pattern)) -> {
      let pattern_len = length_string(pattern)
      let prefix = slice_string(string, 0, position)
      let match_str = slice_string(string, position, pattern_len)
      let suffix = slice_string(string, position + pattern_len, string_len - position - pattern_len)
      #(prefix, match_str, suffix)
    }
    None -> #(string, "", "")
  }
}

pub fn split_before(splitter: Splitter, string: String) -> #(String, String) {
  case split(splitter, string) {
    #(prefix, match_str, suffix) -> #(prefix, match_str <> suffix)
  }
}

pub fn split_after(splitter: Splitter, string: String) -> #(String, String) {
  case split(splitter, string) {
    #(prefix, match_str, suffix) -> #(prefix <> match_str, suffix)
  }
}

fn make(patterns: List(String)) -> Splitter {
  Splitter(patterns)
}

fn find_first_match(patterns: List(String), string: String, start: Int, string_len: Int) -> Option(#(Int, String)) {
  case start >= string_len {
    True -> None
    False -> {
      case check_patterns_at_position(patterns, string, start, string_len) {
        Some(pattern) -> Some(#(start, pattern))
        None -> find_first_match(patterns, string, start + 1, string_len)
      }
    }
  }
}

fn check_patterns_at_position(patterns: List(String), string: String, position: Int, string_len: Int) -> Option(String) {
  case patterns {
    [] -> None
    [pattern, ..rest] -> {
      let pattern_len = length_string(pattern)
      case position + pattern_len <= string_len {
        True -> {
          let substr = slice_string(string, position, pattern_len)
          case substr == pattern {
            True -> Some(pattern)
            False -> check_patterns_at_position(rest, string, position, string_len)
          }
        }
        False -> check_patterns_at_position(rest, string, position, string_len)
      }
    }
  }
}
