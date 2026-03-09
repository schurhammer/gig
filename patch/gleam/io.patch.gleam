import gleam/string

@external(c, "", "gleam_io_do_print")
fn do_print(s: String) -> Nil

@external(c, "", "gleam_io_do_print_error")
fn do_print_error(s: String) -> Nil

@external(c, "", "gleam_io_do_get_line")
fn do_get_line(length: Int) -> String

// Read a line from stdin.
pub fn get_line() -> String {
    let s = do_get_line(1023)
    case string.ends_with(s, "\n") {
        True -> s
        False -> s <> get_line()
    }
}

pub fn print(s: String) -> Nil {
    do_print(s)
}

pub fn println(string: String) -> Nil {
    do_print(string)
    do_print("\n")
}

pub fn println_error(string: String) -> Nil {
  do_print_error(string)
  do_print_error("\n")
}

fn do_debug_println(string string: String) -> Nil {
  println_error(string)
}

pub fn print_error(string: String) -> Nil {
  do_print_error(string)
}

pub fn debug(value: a) -> a {
    println_error(string.inspect(value))
    value
}
