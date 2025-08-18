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

pub fn main() {
  // Underscores in numbers
  echo 1_000_000
  echo 10_000.1

  // Binary literals
  echo 0b00001111
  echo 0b1010
  echo 0b11111111

  // Octal literals
  echo 0o17
  echo 0o777
  echo 0o123

  // Hex literals
  echo 0xF
  echo 0xFF
  echo 0x1A2B

  // Scientific notation Float literals
  echo 7.0e7
  echo 3.0e-4
  echo 1.23e10

  // Regular numbers
  echo 42
  echo 3.14
}
