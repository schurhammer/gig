//// 6

type OptionInt {
  None
  Some(item: Int)
}

type Point {
  Point(x: Int, y: Int, next: OptionInt)
}

pub fn main() {
  let opt = Some(3)
  let a = Point(1, 2, opt)
  echo sum(a)
}

fn sum(p: Point) {
  case p.next {
    None -> p.x + p.y
    Some(item) -> p.x + p.y + item
  }
}
