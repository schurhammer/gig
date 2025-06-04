//// lo
//// wor
//// ld
//// empty
//// http
//// ftp
//// unknown

import gleam/io

pub fn main() {
  // Test basic pattern concatenate
  let assert "hel" <> rest = "hello"
  io.println(rest)

  // Test with binding the prefix too
  let assert "wor" as prefix <> suffix = "world"
  io.println(prefix)
  io.println(suffix)

  // Test with empty suffix
  let assert "test" <> empty = "test"
  case empty {
    "" -> io.println("empty")
    _ -> io.println("not empty")
  }

  // Test case statement with multiple patterns
  test_protocol("http://example.com")
  test_protocol("ftp://files.example.com")
  test_protocol("mailto:test@example.com")
}

fn test_protocol(url: String) {
  case url {
    "http://" <> _ -> io.println("http")
    "https://" <> _ -> io.println("https")
    "ftp://" <> _ -> io.println("ftp")
    _ -> io.println("unknown")
  }
}
