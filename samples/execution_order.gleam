//// 0
//// 1
//// 2
//// 3
//// 4
//// 5
//// 6
//// 7
//// 1
//// 2
//// 3
//// 3
//// 2
//// 1

import gleam/list

fn main() {
  foo(echo 0, echo 1, echo 2)
  let x = {
    echo 4
    echo 5
  }
  echo 6
  echo 7
  list.fold([1, 2, 3], Nil, fn(a, i) {
    echo i
    Nil
  })
  list.fold_right([1, 2, 3], Nil, fn(a, i) {
    echo i
    Nil
  })
}

fn foo(a, b, c) {
  echo 3
  Nil
}
