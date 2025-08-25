pub type Dynamic

import gleam/io
import gleam/string

@external(erlang, "ffi_erlang", "print")
@external(javascript, "./ffi_javascript.mjs", "print")
pub fn print(a: String) -> Nil {
  io.print(a)
}

@external(erlang, "ffi_erlang", "append")
@external(javascript, "./ffi_javascript.mjs", "append")
pub fn append(a: String, b: String) -> String {
  a <> b
}

@external(erlang, "ffi_erlang", "to_string")
@external(javascript, "./ffi_javascript.mjs", "toString")
pub fn to_string(a: anything) -> String {
  string.inspect(a)
}

@external(erlang, "ffi_erlang", "file_exists")
@external(javascript, "./ffi_javascript.mjs", "fileExists")
pub fn file_exists(a: String) -> Bool {
  True
}

@external(erlang, "ffi_erlang", "halt")
@external(javascript, "./ffi_javascript.mjs", "halt")
pub fn halt(a: Int) -> Nil {
  panic
}

pub fn utf_codepoint(a: Int) -> UtfCodepoint {
  string.unsafe_int_to_utf_codepoint(a)
}
