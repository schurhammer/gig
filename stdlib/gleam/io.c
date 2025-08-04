#include "io.h"
#include <stdio.h>

String gets_string(Int max_length) {
  // extra space for null terminator
  max_length += 1;
  char buffer[max_length];
  if (fgets(buffer, max_length, stdin) != NULL) {
    return cstring_to_string(buffer);
  } else {
    return new_String(NULL, 0);
  }
}

Nil gleam_io_do_print(String a0) { return print_string(a0); }

Nil gleam_io_do_print_error(String a0) { return print_string_error(a0); }
