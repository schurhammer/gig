//// 1111111111111

pub fn main() {
  // lt
  let t0 = case 1 < 1 {
    True -> 2
    False -> 1
  }
  let t1 = case 1 < 2 {
    True -> 10
    False -> 20
  }
  let t2 = case 2 < 1 {
    True -> 200
    False -> 100
  }

  // gt
  let t3 = case 1 > 1 {
    True -> 2000
    False -> 1000
  }
  let t4 = case 1 > 2 {
    True -> 20_000
    False -> 10_000
  }
  let t5 = case 2 > 1 {
    True -> 100_000
    False -> 200_000
  }

  // lte
  let t6 = case 1 <= 1 {
    True -> 1_000_000
    False -> 2_000_000
  }
  let t7 = case 1 <= 2 {
    True -> 10_000_000
    False -> 20_000_000
  }
  let t8 = case 2 <= 1 {
    True -> 200_000_000
    False -> 100_000_000
  }
  //gte
  let t9 = case 1 >= 1 {
    True -> 1_000_000_000
    False -> 2_000_000_000
  }
  let t10 = case 1 >= 2 {
    True -> 20_000_000_000
    False -> 10_000_000_000
  }
  let t11 = case 2 >= 1 {
    True -> 100_000_000_000
    False -> 200_000_000_000
  }
  let x = 1
  let y = -x
  let t12 = case y < 0 {
    True -> 1_000_000_000_000
    False -> 2_000_000_000_000
  }
  t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12
}
