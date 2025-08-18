//// [1, 2, 3]
//// 6

pub fn main() {
  echo [1, 2, 3]
  echo sum(range(3))
}

fn sum(l) {
  case l {
    [] -> 0
    [x, ..xs] -> x + sum(xs)
  }
}

fn range(i) {
  case i {
    0 -> []
    i -> [i, ..range(i - 1)]
  }
}
