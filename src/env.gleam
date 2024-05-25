import gleam/io
import gleam/list
import gleam/string

pub opaque type Env(k, v) {
  Env(entries: List(#(k, v)))
}

pub fn new() -> Env(k, v) {
  Env([])
}

pub fn put(e: Env(k, v), k: k, v: v) -> Env(k, v) {
  Env([#(k, v), ..e.entries])
}

pub fn get(e: Env(k, v), k: k) -> Result(v, Nil) {
  case list.find(e.entries, fn(x) { x.0 == k }) {
    Ok(x) -> Ok(x.1)
    Error(_) -> Error(Nil)
  }
}

pub fn get_entry(e: Env(k, v), k: k) -> Result(#(k, v), Nil) {
  list.find(e.entries, fn(x) { x.0 == k })
}

pub fn has(e: Env(k, v), k: k) -> Bool {
  case get(e, k) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn debug(e: Env(k, v)) {
  io.println_error("Env:")
  list.each(e.entries, fn(e) {
    io.println_error(string.inspect(e.0) <> " -> " <> string.inspect(e.1))
  })
  io.println_error("")
}
