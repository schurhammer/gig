#include "builtin.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef GC

#include <gc.h>
#define malloc(x) GC_MALLOC(x)

#else

#include "arena.h"
#define malloc(x) arena_malloc(x)

#endif

_Noreturn Nil panic_exit(String a) {
  print_string(a);
  printf("\n");
  exit(1);
}

Bool eq_Nil(Nil x, Nil y) { return True; }
Bool lt_Nil(Nil x, Nil y) { return False; }

Bool eq_Bool(Bool x, Bool y) { return x == y; }
Bool lt_Bool(Bool x, Bool y) { return x < y; }
Bool and_bool(Bool x, Bool y) { return x && y; }
Bool or_bool(Bool x, Bool y) { return x || y; }
Bool negate_bool(Bool x) { return !x; }
Bool isa_True(Bool x) { return x == True; }
Bool isa_False(Bool x) { return x == False; }
Bool isa_Nil(Nil x) { return True; }

Bool eq_Int(Int x, Int y) { return x == y; }
Bool lt_Int(Int x, Int y) { return x < y; }
Bool gt_Int(Int x, Int y) { return x > y; }
Bool lte_Int(Int x, Int y) { return x <= y; }
Bool gte_Int(Int x, Int y) { return x >= y; }
Int add_Int(Int x, Int y) { return x + y; }
Int sub_Int(Int x, Int y) { return x - y; }
Int mul_Int(Int x, Int y) { return x * y; }
Int div_Int(Int x, Int y) { return x / y; }
Int rem_Int(Int x, Int y) { return x % y; }
Int negate_Int(Int x) { return -x; }

Bool eq_Float(Float x, Float y) { return x == y; }
Bool lt_Float(Float x, Float y) { return x < y; }
Bool gt_Float(Float x, Float y) { return x > y; }
Bool lte_Float(Float x, Float y) { return x <= y; }
Bool gte_Float(Float x, Float y) { return x >= y; }
Float add_Float(Float x, Float y) { return x + y; }
Float sub_Float(Float x, Float y) { return x - y; }
Float mul_Float(Float x, Float y) { return x * y; }
Float div_Float(Float x, Float y) { return x / y; }

Bool eq_UtfCodepoint(UtfCodepoint x, UtfCodepoint y) { return x == y; }
Bool lt_UtfCodepoint(UtfCodepoint x, UtfCodepoint y) { return x < y; }

String inspect_UtfCodepoint(UtfCodepoint value) {
  char buffer[16];
  snprintf(buffer, sizeof(buffer), "%d", value);
  return cstring_to_string(buffer);
}

u_int16_t splice_bits(u_int16_t src, u_int16_t dst, int src_offset,
                      int dst_offset, int n) {
  u_int16_t src_mask = ~0U << (16 - n);
  src_mask >>= src_offset;

  u_int16_t aligned = ((src & src_mask) << src_offset) >> dst_offset;

  u_int16_t dst_mask = ~0U << (16 - n);
  dst_mask >>= dst_offset;

  // printf("%016b %016b %016b %016b %016b %d %d %d\n", src, dst, aligned,
  // src_mask, dst_mask, src_offset, dst_offset, n);

  return (dst & ~dst_mask) | (aligned & dst_mask);
}

BitArray new_bit_array(size_t len) {
  size_t byte_size = (len + 8 - 1) / 8;

  struct BitArray ba;
  ba.bytes = malloc(byte_size);
  if (ba.bytes == NULL) {
    panic_exit(new_String("malloc failed in new_bit_array", -1));
  }
  ba.offset = 0;
  ba.len = len;

  return ba;
}

Nil write_bit_array(BitArray src, BitArray dst, Int offset, Int len) {
  int src_start_bit = src.offset;
  int src_start_byte = src_start_bit / 8;
  int src_bit_offset = src_start_bit % 8;

  int dst_start_bit = dst.offset + offset;
  int dst_start_byte = dst_start_bit / 8;
  int dst_bit_offset = dst_start_bit % 8;

  int byte_len = (len + 8 - 1) / 8;

  for (int i = 0; i < byte_len; i += 1) {

    // read the relevant bytes
    uint16_t a = src.bytes[src_start_byte + i];
    uint16_t b = dst.bytes[dst_start_byte + i];
    if (i + 1 < byte_len) {
      a = (a << 8) | src.bytes[src_start_byte + i + 1];
      b = (b << 8) | dst.bytes[dst_start_byte + i + 1];
    } else {
      a = a << 8;
      b = b << 8;
    }

    int n = len > 8 ? 8 : len;
    u_int16_t c = splice_bits(a, b, src_bit_offset, dst_bit_offset, n);
    len -= 8;

    // write bytes back to dst
    dst.bytes[dst_start_byte + i] = (uint8_t)(c >> 8);
    dst.bytes[dst_start_byte + i + 1] = (uint8_t)(c & 0xFF);
  }

  return 0;
}

Nil write_bit_array_string(String value, BitArray dst, Int offset, Int len) {
  BitArray src;
  src.bytes = (uint8_t *)value.bytes;
  src.offset = 0;
  src.len = 8 * value.byte_length;
  write_bit_array(src, dst, offset, len);
  return 0;
}

Nil write_bit_array_int(Int value, BitArray dst, Int offset, Int len) {
  // convert to big endian
  u_int8_t bytes[8];
  for (int i = 0; i < 8; i++) {
    bytes[i] = value >> (56 - 8 * i);
  }

  BitArray src;
  src.bytes = (uint8_t *)bytes;
  src.offset = 64 - len;
  src.len = len;
  write_bit_array(src, dst, offset, len);
  return 0;
}

String index_bit_array_string(BitArray ba, Int bit_offset, Int bit_length) {
  int byte_length = bit_length / 8;
  String result = new_String(malloc(byte_length), byte_length);
  if (result.bytes == NULL) {
    panic_exit(new_String("malloc failed in index_bit_array_string", -1));
  }

  bit_offset = bit_offset + ba.offset;
  int byte_offset = bit_offset / 8;
  int bits_into_byte = bit_offset % 8;

  for (int i = 0; i < byte_length; i++) {
    uint16_t current_byte = ba.bytes[byte_offset + i];
    uint16_t next_byte = ba.bytes[byte_offset + i + 1];

    // Combine parts of two bytes and shift to correct position
    uint16_t combined = (current_byte << 8) | next_byte;
    combined = (combined << bits_into_byte) >> 8;

    result.bytes[i] = (uint8_t)combined;
  }

  return result;
}

Int index_bit_array_int(BitArray ba, Int bit_offset, Int bit_length) {
  if (bit_length > 64)
    panic_exit(new_String("bit array index too large", -1));

  bit_offset = bit_offset + ba.offset;

  uint64_t result = 0;
  int byte_offset = bit_offset / 8;
  int bits_into_byte = bit_offset % 8;
  int bits_remaining = bit_length;

  for (int i = 0; bits_remaining > 0; i++) {
    uint64_t current_byte = ba.bytes[byte_offset + i];

    if (bits_into_byte > 0) {
      // Combine parts of two bytes
      uint64_t next_byte = ba.bytes[byte_offset + i + 1];
      current_byte = (current_byte << bits_into_byte) |
                     (next_byte >> (8 - bits_into_byte));
    }

    int bits_to_read = 8 - bits_into_byte;
    if (bits_to_read > bits_remaining)
      bits_to_read = bits_remaining;

    result = (result << bits_to_read) |
             ((current_byte >> (8 - bits_into_byte - bits_to_read)) &
              ((1ULL << bits_to_read) - 1));

    bits_remaining -= bits_to_read;
    bits_into_byte = 0;
  }

  return result;
}

BitArray slice_bit_array(BitArray ba, Int offset, Int len) {
  if (len < 0) {
    len = ba.len - offset;
  }

  struct BitArray view;

  view.bytes = ba.bytes;
  view.offset = ba.offset + offset;
  view.len = len;

  return view;
}

Int length_bit_array(BitArray ba) { return ba.len; }

Bool eq_BitArray(BitArray a, BitArray b) {
  if (a.len != b.len) {
    return False;
  }

  if (a.len == 0) {
    return True;
  }

  // Compare bit by bit
  for (size_t i = 0; i < a.len; i++) {
    size_t a_byte_index = (a.offset + i) / 8;
    size_t a_bit_index = (a.offset + i) % 8;
    size_t b_byte_index = (b.offset + i) / 8;
    size_t b_bit_index = (b.offset + i) % 8;

    bool a_bit = (a.bytes[a_byte_index] & (1 << (7 - a_bit_index))) != 0;
    bool b_bit = (b.bytes[b_byte_index] & (1 << (7 - b_bit_index))) != 0;

    if (a_bit != b_bit) {
      return False;
    }
  }

  return True;
}

Bool lt_BitArray(BitArray a, BitArray b) {
  if (a.len < b.len) {
    return True;
  }

  if (a.len > b.len) {
    return False;
  }

  if (a.len == 0) {
    return False;
  }

  // Compare bit by bit
  for (size_t i = 0; i < a.len; i++) {
    size_t a_byte_index = (a.offset + i) / 8;
    size_t a_bit_index = (a.offset + i) % 8;
    size_t b_byte_index = (b.offset + i) / 8;
    size_t b_bit_index = (b.offset + i) % 8;

    bool a_bit = (a.bytes[a_byte_index] & (1 << (7 - a_bit_index))) != 0;
    bool b_bit = (b.bytes[b_byte_index] & (1 << (7 - b_bit_index))) != 0;

    if (a_bit < b_bit) {
      return True;
    }
    if (a_bit > b_bit) {
      return True;
    }
  }

  return True;
}

String new_String(char *bytes, int byte_length) {
  if (byte_length < 0) {
    byte_length = strlen(bytes);
  }
  String str;
  str.byte_length = byte_length;
  str.bytes = bytes;
  if (bytes == NULL) {
    panic_exit(new_String("null string", -1));
  }
  return str;
}

Bool eq_String(String a, String b) {
  if (a.byte_length != b.byte_length) {
    return False;
  }
  return strncmp(a.bytes, b.bytes, a.byte_length) == 0;
}

Bool lt_String(String a, String b) {
  if (a.byte_length < b.byte_length) {
    return True;
  }
  if (a.byte_length > b.byte_length) {
    return False;
  }
  return strncmp(a.bytes, b.bytes, a.byte_length) < 0;
}

Int length_string(String a) { return a.byte_length; }

String append_string(String a, String b) {
  if (a.byte_length == 0) {
    return b;
  }
  if (b.byte_length == 0) {
    return a;
  }
  int byte_length = a.byte_length + b.byte_length;
  String str = new_String(malloc(byte_length), byte_length);
  if (str.bytes == NULL) {
    panic_exit(new_String("malloc failed in append_string", -1));
  }
  memcpy(str.bytes, a.bytes, a.byte_length);
  memcpy(str.bytes + a.byte_length, b.bytes, b.byte_length);
  return str;
}

Bool starts_with_string(String string, String with) {
  if (string.byte_length < with.byte_length) {
    return False;
  }
  for (int i = 0; i < with.byte_length; i++) {
    if (string.bytes[i] != with.bytes[i]) {
      return False;
    }
  }
  return True;
}

Bool ends_with_string(String string, String with) {
  if (string.byte_length < with.byte_length) {
    return False;
  }
  int j = string.byte_length;
  int k = with.byte_length;
  for (int i = 1; i <= with.byte_length; i++) {
    if (string.bytes[j - i] != with.bytes[k - i]) {
      return False;
    }
  }
  return True;
}

String slice_string(String in, Int offset, Int length) {
  if (offset < 0 || length < 0 || offset + length > in.byte_length) {
    return new_String(in.bytes + in.byte_length, 0);
  }
  return new_String(in.bytes + offset, length);
}

String drop_start_string(String string, Int count) {
  if (count <= 0) {
    return string;
  }
  if (count >= string.byte_length) {
    return new_String(string.bytes + string.byte_length, 0);
  }

  return new_String(string.bytes + count, string.byte_length - count);
}

Int compare_string(String str1, String str2) {
  if (str1.byte_length != str2.byte_length) {
    return str1.byte_length < str2.byte_length ? -1 : 1;
  }
  return memcmp(str1.bytes, str2.bytes, str1.byte_length);
}

String cstring_to_string(char *bytes) {
  int byte_length = strlen(bytes);
  String str = new_String(malloc(byte_length), byte_length);
  if (str.bytes == NULL) {
    panic_exit(new_String("malloc failed in cstring_to_string", -1));
  }
  memcpy(str.bytes, bytes, str.byte_length);
  return str;
}

Int print_string(String a) {
  printf("%.*s", a.byte_length, a.bytes);
  return 0;
}

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

String inspect_Nil(Nil value) { return new_String("Nil", 3); }

String inspect_Bool(Bool b) {
  if (b)
    return new_String("True", 4);
  else
    return new_String("False", 5);
}

String inspect_Int(Int value) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%ld", value);
  return cstring_to_string(buffer);
}

String inspect_Float(Float value) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%g", value);
  return cstring_to_string(buffer);
}

String inspect_String(String s) {
  // escape special characters
  char ooh[256] = {0};
  ooh['\a'] = 'n';
  ooh['\b'] = 'b';
  ooh['\f'] = 'f';
  ooh['\n'] = 'n';
  ooh['\r'] = 'r';
  ooh['\t'] = 't';
  ooh['\v'] = 'v';
  ooh['\\'] = '\\';
  ooh['"'] = '"';

  char buffer[s.byte_length * 2 + 3];
  size_t bp = 0;
  buffer[bp++] = '"';
  for (size_t sp = 0; sp < s.byte_length; sp++) {
    if (ooh[s.bytes[sp]]) {
      buffer[bp++] = '\\';
      buffer[bp++] = ooh[s.bytes[sp]];
    } else {
      buffer[bp++] = s.bytes[sp];
    }
  }
  buffer[bp++] = '"';
  buffer[bp] = 0;
  return cstring_to_string(buffer);
}

String inspect_BitArray(struct BitArray ba) {
  // Calculate the number of bytes
  size_t num_bytes = (ba.len + 7) / 8;

  // Buffer size: 3 chars per byte (max 255), plus commas, brackets, and null
  // terminator
  char buffer[num_bytes * 4 + 5];
  size_t bp = 0;

  buffer[bp++] = '<';
  buffer[bp++] = '<';

  for (size_t i = 0; i < num_bytes; i++) {
    if (i > 0) {
      buffer[bp++] = ',';
    }

    // Calculate the start and end bit for this byte
    size_t start_bit = i * 8;
    size_t end_bit = (start_bit + 8 > ba.len) ? ba.len : start_bit + 8;

    // Extract the byte
    unsigned char byte = 0;
    for (size_t j = start_bit; j < end_bit; j++) {
      size_t byte_index = (ba.offset + j) / 8;
      size_t bit_index = (ba.offset + j) % 8;

      if (ba.bytes[byte_index] & (1 << (7 - bit_index))) {
        byte |= (1 << (7 - (j - start_bit)));
      }
    }

    // Convert byte to decimal string
    bp += sprintf(&buffer[bp], "%d", byte);
  }

  buffer[bp++] = '>';
  buffer[bp++] = '>';
  buffer[bp] = '\0';

  return cstring_to_string(buffer);
}

String inspect_Closure(Closure c) { return new_String("Closure", 7); }

Closure create_closure(void *fun, void *env) {
  struct Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = env;
  return RETURN;
}

Closure create_function(void *fun) {
  struct Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = 0;
  return RETURN;
}

Bool is_closure(Closure c) { return c.env != 0; }

Bool eq_Closure(Closure a, Closure b) { return False; }
Bool lt_Closure(Closure a, Closure b) { return a.fun < b.fun; }

// Global variables to store command line arguments
int global_argc = 0;
char **global_argv = NULL;

void init(int argc, char **argv) {
#ifdef GC
  GC_INIT();
#endif

  global_argc = argc;
  global_argv = argv;
}
