#include "int.h"
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Int gleam_int_bitwise_and(Int a0, Int a1) { return a0 & a1; }

Int gleam_int_bitwise_exclusive_or(Int a0, Int a1) { return a0 ^ a1; }

Int gleam_int_bitwise_not(Int a0) { return ~a0; }

Int gleam_int_bitwise_or(Int a0, Int a1) { return a0 | a1; }

Int gleam_int_bitwise_shift_left(Int a0, Int a1) { return a0 << a1; }

Int gleam_int_bitwise_shift_right(Int a0, Int a1) { return a0 >> a1; }

Result_Int_Nil gleam_int_do_base_parse(String str, Int base) {
  // Check for empty string
  if (str.byte_length == 0) {
    return new_Error_Int_Nil(0);
  }

  // Check for valid base range
  if (base < 2 || base > 36) {
    return new_Error_Int_Nil(0);
  }

  // Convert Gleam String to C string
  char *c_str = malloc(str.byte_length + 1);
  if (!c_str) {
    return new_Error_Int_Nil(0);
  }

  memcpy(c_str, str.bytes, str.byte_length);
  c_str[str.byte_length] = '\0';

  // Parse the string
  char *endptr;
  errno = 0;
  long long result = strtoll(c_str, &endptr, (int)base);

  // Check for parsing errors
  Bool parse_error = False;

  // Check if no digits were parsed
  if (endptr == c_str) {
    parse_error = True;
  }

  // Check if there are remaining characters
  if (*endptr != '\0') {
    parse_error = True;
  }

  // Check for overflow/underflow
  if (errno == ERANGE) {
    parse_error = True;
  }

  // Check if result fits in Int (int64_t) - this is already guaranteed since
  // result is long long and Int is int64_t, but we check for safety

  free(c_str);

  if (parse_error) {
    return new_Error_Int_Nil(0);
  }

  return new_Ok_Int_Nil((Int)result);
}

Result_Int_Nil gleam_int_parse(String str) {
  return gleam_int_do_base_parse(str, 10);
}

String gleam_int_do_to_base_string(Int value, Int base) {
  // Check if base is valid
  if (base < 2 || base > 36) {
    // Return an empty string for invalid base
    return new_String("", 0);
  }

  // Calculate the maximum size needed for int64_t
  char buffer[70];

  // Use signed long long to handle negative numbers
  int len = snprintf(
      buffer, sizeof(buffer),
      base == 10 ? "%lld"
                 : (base == 16 ? "%llx" : (base == 2 ? "%llb" : "%llo")),
      (long long)value);

  // Return the string using cstring_to_string
  return cstring_to_string(buffer);
}

Float gleam_int_to_float(Int value) { return (Float)value; }

String gleam_int_to_string(Int value) {
  // Calculate the maximum size needed for int64_t
  char buffer[32];

  int len = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);

  // Return the string using cstring_to_string
  return cstring_to_string(buffer);
}
