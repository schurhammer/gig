//// 21

type Option(a) {
  None
  Some(item: a)
}

type Point {
  Point(x: Int, y: Int, next: Option(Point))
}

pub fn main() {
  let a = Point(1, 2, Some(Point(3, 4, Some(Point(5, 6, None)))))
  echo sum(a)
}

fn sum(p: Point) {
  case p.next {
    None -> p.x + p.y
    Some(next) -> p.x + p.y + sum(next)
  }
}
