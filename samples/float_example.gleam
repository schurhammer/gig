//// 0.2
//// 0.3
//// 0.1
//// 0.02
//// 2
//// 12345678912345

pub fn main() {
  let x = 0.1
  let y = 0.1
  let z = 0.2
  print(inspect(z) <> "\n")
  print(inspect(z +. x) <> "\n")
  print(inspect(z -. x) <> "\n")
  print(inspect(z *. x) <> "\n")
  print(inspect(z /. x) <> "\n")
  case x == y {
    True -> print("1")
    False -> print("0")
  }
  case x == z {
    True -> print("0")
    False -> print("2")
  }
  case x <. y {
    True -> print("0")
    False -> print("3")
  }
  case x <. z {
    True -> print("4")
    False -> print("0")
  }
  case z <. x {
    True -> print("0")
    False -> print("5")
  }
  case x >. y {
    True -> print("0")
    False -> print("6")
  }
  case x >. z {
    True -> print("0")
    False -> print("7")
  }
  case z >. x {
    True -> print("8")
    False -> print("0")
  }
  case x <=. y {
    True -> print("9")
    False -> print("0")
  }
  case x <=. z {
    True -> print("1")
    False -> print("0")
  }
  case z <=. x {
    True -> print("0")
    False -> print("2")
  }
  case x >=. y {
    True -> print("3")
    False -> print("0")
  }
  case x >=. z {
    True -> print("0")
    False -> print("4")
  }
  case z >=. x {
    True -> print("5")
    False -> print("0")
  }
}
