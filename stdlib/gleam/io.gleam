import gleam/string

@external(c, "", "print_string")
fn do_print(s: String) -> Int

@external(c, "", "gets_string")
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
    Nil
}

pub fn println(s: String) -> Nil {
    do_print(s)
    do_print("\n")
    Nil
}

pub fn debug(value: a) -> a {
    println(string.inspect(value))
    value
}
