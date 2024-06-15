//// 0.2
//// 0.3
//// 0.1
//// 0.02
//// 2
//// 12345678912345

import gleam/io

pub fn main() {
  let x = 0.1
  let y = 0.1
  let z = 0.2
  io.debug(z)
  io.debug(z +. x)
  io.debug(z -. x)
  io.debug(z *. x)
  io.debug(z /. x)
  case x == y {
    True -> io.print("1")
    False -> io.print("0")
  }
  case x == z {
    True -> io.print("0")
    False -> io.print("2")
  }
  case x <. y {
    True -> io.print("0")
    False -> io.print("3")
  }
  case x <. z {
    True -> io.print("4")
    False -> io.print("0")
  }
  case z <. x {
    True -> io.print("0")
    False -> io.print("5")
  }
  case x >. y {
    True -> io.print("0")
    False -> io.print("6")
  }
  case x >. z {
    True -> io.print("0")
    False -> io.print("7")
  }
  case z >. x {
    True -> io.print("8")
    False -> io.print("0")
  }
  case x <=. y {
    True -> io.print("9")
    False -> io.print("0")
  }
  case x <=. z {
    True -> io.print("1")
    False -> io.print("0")
  }
  case z <=. x {
    True -> io.print("0")
    False -> io.print("2")
  }
  case x >=. y {
    True -> io.print("3")
    False -> io.print("0")
  }
  case x >=. z {
    True -> io.print("0")
    False -> io.print("4")
  }
  case z >=. x {
    True -> io.print("5")
    False -> io.print("0")
  }
}
