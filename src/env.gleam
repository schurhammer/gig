import gleam/dict
import gleam/io
import gleam/string

pub opaque type Env(k, v) {
  Env(entries: dict.Dict(k, v))
}

pub fn new() -> Env(k, v) {
  Env(dict.new())
}

pub fn put(e: Env(k, v), k: k, v: v) -> Env(k, v) {
  dict.insert(e.entries, k, v)
  Env(dict.insert(e.entries, k, v))
}

pub fn get(e: Env(k, v), k: k) -> Result(v, Nil) {
  dict.get(e.entries, k)
}

pub fn get_entry(e: Env(k, v), k: k) -> Result(#(k, v), Nil) {
  case dict.get(e.entries, k) {
    Ok(v) -> Ok(#(k, v))
    Error(e) -> Error(e)
  }
}

pub fn has(e: Env(k, v), k: k) -> Bool {
  case get(e, k) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn debug(e: Env(k, v)) {
  io.println_error("Env:")
  dict.fold(e.entries, Nil, fn(_, k, v) {
    io.println_error(string.inspect(k) <> " -> " <> string.inspect(v))
  })
  io.println_error("")
}
