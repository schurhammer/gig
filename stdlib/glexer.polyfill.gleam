@external(c, "", "slice_string")
fn slice_bytes(string: String, from byte: Int, sized bytes: Int) -> String

fn drop_byte(string: String) -> String {
  drop_start_string(string, 1)
}
