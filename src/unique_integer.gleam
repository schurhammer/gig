type Mod {
  Monotonic
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(mods: List(Mod)) -> Int

pub fn any() -> Int {
  unique_integer([])
}

pub fn mono() -> Int {
  unique_integer([Monotonic])
}

pub fn positive() {
  unique_integer([Positive])
}

pub fn mono_positive() -> Int {
  unique_integer([Monotonic, Positive])
}
