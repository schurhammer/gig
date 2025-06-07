//// 1000000
//// 10000.1
//// 15
//// 10
//// 255
//// 15
//// 511
//// 83
//// 15
//// 255
//// 6699
//// 7e+07
//// 0.0003
//// 1.23e+10
//// 42
//// 3.14

import gleam/io

pub fn main() {
  // Underscores in numbers
  io.debug(1_000_000)
  io.debug(10_000.1)

  // Binary literals
  io.debug(0b00001111)
  io.debug(0b1010)
  io.debug(0b11111111)

  // Octal literals
  io.debug(0o17)
  io.debug(0o777)
  io.debug(0o123)

  // Hex literals
  io.debug(0xF)
  io.debug(0xFF)
  io.debug(0x1A2B)

  // Scientific notation Float literals
  io.debug(7.0e7)
  io.debug(3.0e-4)
  io.debug(1.23e10)

  // Regular numbers
  io.debug(42)
  io.debug(3.14)
}
