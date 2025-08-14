#include "bit_array.h"
#include <stdint.h>

BitArray gleam_bit_array_from_string(String s) {
  BitArray a;
  a.bytes = (uint8_t *)s.bytes;
  a.len = 8 * s.byte_length;
  a.offset = 0;
  return a;
}
