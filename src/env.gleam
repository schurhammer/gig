import gleam/list

pub opaque type Env(k, v) {
  Env(entries: List(#(k, v)))
}

pub fn new() -> Env(k, v) {
  new()
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

pub fn has(e: Env(k, v), k: k) -> Bool {
  case get(e, k) {
    Ok(_) -> True
    Error(_) -> False
  }
}
