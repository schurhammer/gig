import gleam/order

pub fn compare(a: Int, b: Int) -> order.Order {
  case a - b {
    x if x < 0 -> order.Lt
    x if x > 0 -> order.Gt
    _ -> order.Eq
  }
}
