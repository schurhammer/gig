//// anon_Unselected

import gleam/io

pub type Status {
  Selected
  Unselected
}

pub fn main() {
  let toggle = fn(status) {
    case status {
      Selected -> Unselected
      Unselected -> Selected
    }
  }

  let do_toggle = fn(status) { toggle(status) }

  io.debug(do_toggle(Selected))
}
