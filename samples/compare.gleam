//// False
//// True
//// False
//// False
//// False
//// True
//// False
//// False
//// True
//// False
//// True
//// False
//// True
//// False

import gleam/io

type Point {
  Point(x: Int, y: Int)
}

type Foo {
  Bar(x: Int)
  Baz(x: Int)
}

pub fn main() {
  io.debug(lt(1, 1))
  io.debug(lt(1, 2))
  io.debug(lt(2, 1))

  let a = Point(1, 2)
  let b = Point(1, 2)
  let c = Point(1, 3)

  io.debug(lt(a, b))
  io.debug(lt(b, a))
  io.debug(lt(a, c))
  io.debug(lt(c, a))

  let a = Bar(1)
  let b = Bar(2)
  let c = Baz(1)

  io.debug(lt(a, a))
  io.debug(lt(a, b))
  io.debug(lt(b, a))
  io.debug(lt(a, c))
  io.debug(lt(c, a))
  io.debug(lt(b, c))
  io.debug(lt(c, b))
}
