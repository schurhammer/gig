//// "Bob"

type Animal {
  Cat(name: String)
  Fox(name: String)
}

pub fn main() {
  let cat = Cat("Bob")
  echo cat.name
}
