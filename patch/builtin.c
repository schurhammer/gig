#include "builtin.h"
#include <endian.h>
#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

Bool eq_Int(Int x, Int y) { return x == y; }
Bool lt_Int(Int x, Int y) { return x < y; }
Bool gt_Int(Int x, Int y) { return x > y; }
Bool lte_Int(Int x, Int y) { return x <= y; }
Bool gte_Int(Int x, Int y) { return x >= y; }
Int add_Int(Int x, Int y) { return x + y; }
Int sub_Int(Int x, Int y) { return x - y; }
Int mul_Int(Int x, Int y) { return x * y; }
Int div_Int(Int x, Int y) { return y == 0 ? 0 : x / y; }
Int rem_Int(Int x, Int y) { return y == 0 ? 0 : x % y; }
Int negate_Int(Int x) { return -x; }

Bool eq_Float(Float x, Float y) { return x == y; }
Bool lt_Float(Float x, Float y) { return x < y; }
Bool gt_Float(Float x, Float y) { return x > y; }
Bool lte_Float(Float x, Float y) { return x <= y; }
Bool gte_Float(Float x, Float y) { return x >= y; }
Float add_Float(Float x, Float y) { return x + y; }
Float sub_Float(Float x, Float y) { return x - y; }
Float mul_Float(Float x, Float y) { return x * y; }
Float div_Float(Float x, Float y) { return y == 0.0 ? 0.0 : x / y; }

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

  return (dst & ~dst_mask) | (aligned & dst_mask);
}

BitArray new_bit_array(Int len) {
  struct BitArray ba;

  if (len <= 0) {
    ba.bytes = NULL;
    ba.offset = 0;
    ba.len = 0;
    return ba;
  }

  ba.bytes = malloc((len + 7) / 8);
  ba.offset = 0;
  ba.len = len;

  if (ba.bytes == NULL) {
    panic_exit(new_String("malloc failed in new_bit_array", -1));
  }

  return ba;
}

Int length_bit_array(BitArray ba) { return ba.len; }

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

Nil write_bit_array(BitArray src, BitArray dst, Int offset, Int len) {
  int src_start_bit = src.offset;
  int src_start_byte = src_start_bit / 8;
  int src_bit_offset = src_start_bit % 8;

  int dst_start_bit = dst.offset + offset;
  int dst_start_byte = dst_start_bit / 8;
  int dst_bit_offset = dst_start_bit % 8;

  int byte_len = (len + 7) / 8;

  for (int i = 0; i < byte_len; i += 1) {
    // read the relevant bytes
    uint16_t a = src.bytes[src_start_byte + i];
    uint16_t b = dst.bytes[dst_start_byte + i];

    if (src.len > (i + 1) * 8) {
      a = (a << 8) | src.bytes[src_start_byte + i + 1];
    } else {
      a = a << 8;
    }
    b = b << 8;

    int n = len > 8 ? 8 : len;
    u_int16_t c = splice_bits(a, b, src_bit_offset, dst_bit_offset, n);
    len -= 8;

    // write bytes back to dst
    dst.bytes[dst_start_byte + i] = (uint8_t)(c >> 8);
    if (dst.len > (i + 1) * 8) {
      dst.bytes[dst_start_byte + i + 1] = (uint8_t)(c & 0xFF);
    }
  }

  return 0;
}

Nil write_bit_array_utf8_string(String value, BitArray dst, Int offset, Int len,
                                Int endian) {
  BitArray src;
  src.bytes = (uint8_t *)value.bytes;
  src.offset = 0;
  src.len = 8 * value.byte_length;
  write_bit_array(src, dst, offset, src.len);
  return 0;
}

Nil write_bit_array_utf16_string(String value, BitArray dst, Int offset,
                                 Int len, Int endian) {
  for (int i = 0, bit_pos = 0; i < value.byte_length;) {
    uint8_t first_byte = value.bytes[i];
    UtfCodepoint codepoint;
    int bytes_used;

    if (first_byte < 0x80) {
      codepoint = first_byte;
      bytes_used = 1;
    } else if ((first_byte & 0xE0) == 0xC0) {
      codepoint = ((first_byte & 0x1F) << 6) | (value.bytes[i + 1] & 0x3F);
      bytes_used = 2;
    } else if ((first_byte & 0xF0) == 0xE0) {
      codepoint = ((first_byte & 0x0F) << 12) |
                  ((value.bytes[i + 1] & 0x3F) << 6) |
                  (value.bytes[i + 2] & 0x3F);
      bytes_used = 3;
    } else if ((first_byte & 0xF8) == 0xF0) {
      codepoint =
          ((first_byte & 0x07) << 18) | ((value.bytes[i + 1] & 0x3F) << 12) |
          ((value.bytes[i + 2] & 0x3F) << 6) | (value.bytes[i + 3] & 0x3F);
      bytes_used = 4;
    } else {
      bytes_used = 1;
      i += bytes_used;
      continue;
    }

    if (codepoint <= 0xFFFF) {
      write_bit_array_int(codepoint, dst, offset + bit_pos, 16, endian);
      bit_pos += 16;
    } else {
      codepoint -= 0x10000;
      uint16_t high_surrogate = 0xD800 + (codepoint >> 10);
      uint16_t low_surrogate = 0xDC00 + (codepoint & 0x3FF);
      write_bit_array_int(high_surrogate, dst, offset + bit_pos, 16, endian);
      bit_pos += 16;
      write_bit_array_int(low_surrogate, dst, offset + bit_pos, 16, endian);
      bit_pos += 16;
    }
    i += bytes_used;
  }

  return 0;
}

Nil write_bit_array_utf32_string(String value, BitArray dst, Int offset,
                                 Int len, Int endian) {
  // Convert UTF-8 string to UTF-32 encoding
  int byte_length = value.byte_length;

  for (int i = 0, bit_pos = 0; i < byte_length;) {
    uint8_t first_byte = value.bytes[i];
    UtfCodepoint codepoint;
    int bytes_used;

    if (first_byte < 0x80) {
      codepoint = first_byte;
      bytes_used = 1;
    } else if ((first_byte & 0xE0) == 0xC0) {
      codepoint = ((first_byte & 0x1F) << 6) | (value.bytes[i + 1] & 0x3F);
      bytes_used = 2;
    } else if ((first_byte & 0xF0) == 0xE0) {
      codepoint = ((first_byte & 0x0F) << 12) |
                  ((value.bytes[i + 1] & 0x3F) << 6) |
                  (value.bytes[i + 2] & 0x3F);
      bytes_used = 3;
    } else if ((first_byte & 0xF8) == 0xF0) {
      codepoint =
          ((first_byte & 0x07) << 18) | ((value.bytes[i + 1] & 0x3F) << 12) |
          ((value.bytes[i + 2] & 0x3F) << 6) | (value.bytes[i + 3] & 0x3F);
      bytes_used = 4;
    } else {
      bytes_used = 1;
      i += bytes_used;
      continue;
    }

    write_bit_array_int(codepoint, dst, offset + bit_pos, 32, endian);
    bit_pos += 32;
    i += bytes_used;
  }

  return 0;
}

union IndexBuffer {
  uint8_t bytes[8];
  uint64_t u64;
  uint32_t u32;
  uint16_t u16;
  uint8_t u8;
  int64_t i64;
  double f64;
  float f32;
};

static uint64_t endian_swap(uint64_t value) {
  return ((value >> 56) & 0xff) | ((value >> 40) & 0xff00) |
         ((value >> 24) & 0xff0000) | ((value >> 8) & 0xff000000) |
         ((value << 8) & 0xff00000000ULL) |
         ((value << 24) & 0xff0000000000ULL) |
         ((value << 40) & 0xff000000000000ULL) |
         ((value << 56) & 0xff00000000000000ULL);
}

static int64_t sign_extend(uint64_t value, int bit_length) {
  if (bit_length < 64) {
    uint64_t mask = 1ULL << (bit_length - 1);
    if (value & mask) {
      // Negative number, sign extend
      value |= ~((1ULL << bit_length) - 1);
    }
  }
  return (int64_t)value;
}

uint16_t float_to_half(float f) {
  uint32_t bits;
  memcpy(&bits, &f, sizeof(f));

  uint32_t sign = (bits >> 31) & 0x1;
  int32_t exp = (bits >> 23) & 0xFF;
  uint32_t frac = bits & 0x7FFFFF;

  uint16_t h;

  if (exp == 255) { // Inf or NaN
    if (frac == 0) {
      h = (sign << 15) | (0x1F << 10); // Inf
    } else {
      h = (sign << 15) | (0x1F << 10) | (frac ? 0x200 : 0); // NaN
    }
  } else if (exp > 142) {
    // Overflow -> Inf
    h = (sign << 15) | (0x1F << 10);
  } else if (exp < 113) {
    // Subnormal or zero
    if (exp < 103) {
      // Too small -> zero
      h = (sign << 15);
    } else {
      // Subnormal half
      uint32_t mant = (frac | 0x800000) >> (113 - exp);
      h = (sign << 15) | (mant >> 13);
    }
  } else {
    // Normalized half
    uint16_t new_exp = exp - 112;
    h = (sign << 15) | (new_exp << 10) | (frac >> 13);
  }

  return h;
}

// Convert IEEE 754 half -> float
float half_to_float(uint16_t h) {
  uint32_t sign = (h >> 15) & 0x1;
  uint32_t exp = (h >> 10) & 0x1F;
  uint32_t frac = h & 0x3FF;

  uint32_t bits;

  if (exp == 0) {
    if (frac == 0) {
      // Zero
      bits = sign << 31;
    } else {
      // Subnormal -> normalized float
      exp = 113; // (127 - 15 + 1)
      while ((frac & 0x400) == 0) {
        frac <<= 1;
        exp--;
      }
      frac &= 0x3FF;
      bits = (sign << 31) | (exp << 23) | (frac << 13);
    }
  } else if (exp == 0x1F) {
    // Inf or NaN
    bits = (sign << 31) | (0xFF << 23) | (frac << 13);
  } else {
    // Normalized
    uint32_t new_exp = exp + (127 - 15);
    bits = (sign << 31) | (new_exp << 23) | (frac << 13);
  }

  float f;
  memcpy(&f, &bits, sizeof(f));
  return f;
}

Nil write_bit_array_value(union IndexBuffer value, BitArray dst, Int offset,
                          Int len, enum Endian endian) {
  if (len <= 0) {
    return 0;
  }

  BitArray src;
  src.bytes = value.bytes;

  int native_endian = BYTE_ORDER == BIG_ENDIAN ? BIG : LITTLE;
  bool swap = (endian != NATIVE) && (endian != native_endian);

  if (swap) {
    value.u64 = endian_swap(value.u64);
    src.offset = 64 - len;
  } else {
    src.offset = 0;
  }

  src.len = len;
  write_bit_array(src, dst, offset, len);

  return 0;
}

union IndexBuffer index_bit_array_value(BitArray ba, Int bit_offset,
                                        Int bit_length, enum Endian endian) {
  // configure source bit array
  ba.offset = ba.offset + bit_offset;

  union IndexBuffer buffer;
  buffer.u64 = 0;

  // configure destination bit array to write into buffer
  BitArray dst;
  dst.bytes = buffer.bytes;
  dst.len = bit_length;
  dst.offset = 0;
  // dst.offset = (byte_len * 8) - bit_length;
  // dst.offset = (8 - (bit_length % 8)) % 8;

  // use write_bit_array to write into buffer
  write_bit_array(ba, dst, 0, bit_length);

  // adjust endianness if needed
  int native_endian = BYTE_ORDER == BIG_ENDIAN ? BIG : LITTLE;
  bool swap = (endian != NATIVE) && (endian != native_endian);

  if (swap) {
    buffer.u64 = endian_swap(buffer.u64) >> (64 - bit_length);
  } else {
    int byte_len = (bit_length + 7) / 8;
    uint8_t *b = &dst.bytes[byte_len - 1];
    *b = *b >> ((8 - (bit_length % 8)) % 8);
  }

  return buffer;
}

Nil write_bit_array_int(Int value, BitArray dst, Int offset, Int len,
                        Int endian) {
  union IndexBuffer buffer;
  buffer.i64 = value;
  return write_bit_array_value(buffer, dst, offset, len, endian);
}

Nil write_bit_array_float(Float value, BitArray dst, Int offset, Int len,
                          Int endian) {
  union IndexBuffer buffer;
  if (len == 64) {
    buffer.f64 = value;
  } else if (len == 32) {
    buffer.f32 = value;
  } else if (len == 16) {
    buffer.u16 = float_to_half(value);
  } else {
    panic_exit(new_String("unsupported float size", 22));
  }
  return write_bit_array_value(buffer, dst, offset, len, endian);
}

Int index_bit_array_int(BitArray ba, Int bit_offset, Int bit_length,
                        Int endian) {
  return index_bit_array_value(ba, bit_offset, bit_length, endian).u64;
}

Int index_bit_array_int_signed(BitArray ba, Int bit_offset, Int bit_length,
                               Int endian) {
  union IndexBuffer buffer =
      index_bit_array_value(ba, bit_offset, bit_length, endian);
  return sign_extend(buffer.u64, bit_length);
}

Float index_bit_array_float(BitArray ba, Int bit_offset, Int bit_length,
                            Int endian) {
  if (bit_length == 64) {
    return index_bit_array_value(ba, bit_offset, bit_length, endian).f64;
  }
  if (bit_length == 32) {
    return index_bit_array_value(ba, bit_offset, bit_length, endian).f32;
  }
  if (bit_length == 16) {
    uint16_t f = index_bit_array_value(ba, bit_offset, bit_length, endian).u16;
    return half_to_float(f);
  }
  panic_exit(new_String("unsupported float size", 22));
}

UtfCodepoint index_bit_array_utf8(BitArray ba, Int bit_offset, Int endian) {
  // Read first byte to determine UTF-8 sequence length
  uint8_t first_byte = index_bit_array_value(ba, bit_offset, 8, endian).u8;

  if (first_byte < 0x80) {
    // 1-byte sequence: 0xxxxxxx
    return first_byte;
  } else if ((first_byte & 0xE0) == 0xC0) {
    // 2-byte sequence: 110xxxxx 10xxxxxx
    uint8_t second_byte =
        index_bit_array_value(ba, bit_offset + 8, 8, endian).u8;
    return ((first_byte & 0x1F) << 6) | (second_byte & 0x3F);
  } else if ((first_byte & 0xF0) == 0xE0) {
    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
    uint8_t second_byte =
        index_bit_array_value(ba, bit_offset + 8, 8, endian).u8;
    uint8_t third_byte =
        index_bit_array_value(ba, bit_offset + 16, 8, endian).u8;
    return ((first_byte & 0x0F) << 12) | ((second_byte & 0x3F) << 6) |
           (third_byte & 0x3F);
  } else if ((first_byte & 0xF8) == 0xF0) {
    // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx
    // 10xxxxxx
    uint8_t second_byte =
        index_bit_array_value(ba, bit_offset + 8, 8, endian).u8;
    uint8_t third_byte =
        index_bit_array_value(ba, bit_offset + 16, 8, endian).u8;
    uint8_t fourth_byte =
        index_bit_array_value(ba, bit_offset + 24, 8, endian).u8;
    return ((first_byte & 0x07) << 18) | ((second_byte & 0x3F) << 12) |
           ((third_byte & 0x3F) << 6) | (fourth_byte & 0x3F);
  }

  // Invalid UTF-8 sequence, return replacement
  // character
  return 0xFFFD;
}

UtfCodepoint index_bit_array_utf16(BitArray ba, Int bit_offset, Int endian) {
  uint16_t code_unit = index_bit_array_value(ba, bit_offset, 16, endian).u16;

  // Check if this is a high surrogate
  if (code_unit >= 0xD800 && code_unit <= 0xDBFF) {
    // High surrogate - read the next 16 bits for low
    // surrogate
    uint16_t low_surrogate =
        index_bit_array_value(ba, bit_offset + 16, 16, endian).u16;
    if (low_surrogate >= 0xDC00 && low_surrogate <= 0xDFFF) {
      // Valid surrogate pair
      return 0x10000 + ((code_unit - 0xD800) << 10) + (low_surrogate - 0xDC00);
    }
  }

  // Single code unit or invalid surrogate (return as-is
  // for now)
  return code_unit;
}

UtfCodepoint index_bit_array_utf32(BitArray ba, Int bit_offset, Int endian) {
  return index_bit_array_value(ba, bit_offset, 32, endian).u32;
}

String index_bit_array_utf8_string(BitArray ba, Int bit_offset, Int bit_length,
                                   Int endian) {
  int byte_length = bit_length / 8;
  String result = new_String(malloc(byte_length), byte_length);
  if (result.bytes == NULL) {
    panic_exit(new_String("malloc failed in index utf8", -1));
  }

  bit_offset = bit_offset + ba.offset;
  int byte_offset = bit_offset / 8;
  int bits_into_byte = bit_offset % 8;

  for (int i = 0; i < byte_length; i++) {
    uint16_t current_byte = ba.bytes[byte_offset + i];
    uint16_t next_byte = ba.bytes[byte_offset + i + 1];

    // Combine parts of two bytes and shift to correct
    // position
    uint16_t combined = (current_byte << 8) | next_byte;
    combined = (combined << bits_into_byte) >> 8;

    result.bytes[i] = (uint8_t)combined;
  }

  return result;
}

String index_bit_array_utf16_string(BitArray ba, Int bit_offset, Int bit_length,
                                    Int endian) {
  // Convert UTF-16 to UTF-8 string
  int code_units = bit_length / 16;
  int max_utf8_length = code_units * 4; // Worst case: 4 bytes per code unit
  String result = new_String(malloc(max_utf8_length), 0);
  if (result.bytes == NULL) {
    panic_exit(new_String("malloc failed in index_bit_array_utf16", -1));
  }

  int utf8_pos = 0;
  for (int i = 0; i < code_units; i++) {
    uint16_t code_unit =
        index_bit_array_value(ba, bit_offset + i * 16, 16, endian).u16;

    if (code_unit >= 0xD800 && code_unit <= 0xDBFF) {
      // High surrogate - read low surrogate
      uint16_t low_surrogate =
          index_bit_array_value(ba, bit_offset + (i + 1) * 16, 16, endian).u16;
      if (low_surrogate >= 0xDC00 && low_surrogate <= 0xDFFF) {
        // Valid surrogate pair
        UtfCodepoint codepoint =
            0x10000 + ((code_unit - 0xD800) << 10) + (low_surrogate - 0xDC00);
        i++; // Skip the low surrogate

        // Encode to UTF-8
        if (codepoint <= 0x7F) {
          result.bytes[utf8_pos++] = codepoint;
        } else if (codepoint <= 0x7FF) {
          result.bytes[utf8_pos++] = 0xC0 | (codepoint >> 6);
          result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
        } else if (codepoint <= 0xFFFF) {
          result.bytes[utf8_pos++] = 0xE0 | (codepoint >> 12);
          result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 6) & 0x3F);
          result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
        } else {
          result.bytes[utf8_pos++] = 0xF0 | (codepoint >> 18);
          result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 12) & 0x3F);
          result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 6) & 0x3F);
          result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
        }
      } else {
        // Invalid surrogate, use replacement character
        result.bytes[utf8_pos++] = 0xEF;
        result.bytes[utf8_pos++] = 0xBF;
        result.bytes[utf8_pos++] = 0xBD;
      }
    } else if (code_unit >= 0xDC00 && code_unit <= 0xDFFF) {
      // Unexpected low surrogate, use replacement
      // character
      result.bytes[utf8_pos++] = 0xEF;
      result.bytes[utf8_pos++] = 0xBF;
      result.bytes[utf8_pos++] = 0xBD;
    } else {
      // Regular code unit
      if (code_unit <= 0x7F) {
        result.bytes[utf8_pos++] = code_unit;
      } else if (code_unit <= 0x7FF) {
        result.bytes[utf8_pos++] = 0xC0 | (code_unit >> 6);
        result.bytes[utf8_pos++] = 0x80 | (code_unit & 0x3F);
      } else {
        result.bytes[utf8_pos++] = 0xE0 | (code_unit >> 12);
        result.bytes[utf8_pos++] = 0x80 | ((code_unit >> 6) & 0x3F);
        result.bytes[utf8_pos++] = 0x80 | (code_unit & 0x3F);
      }
    }
  }

  result.byte_length = utf8_pos;
  return result;
}

String index_bit_array_utf32_string(BitArray ba, Int bit_offset, Int bit_length,
                                    Int endian) {
  // Convert UTF-32 to UTF-8 string
  int code_points = bit_length / 32;
  int max_utf8_length = code_points * 4;
  String result = new_String(malloc(max_utf8_length), 0);
  if (result.bytes == NULL) {
    panic_exit(new_String("malloc failed in index_bit_array_utf32", -1));
  }

  int utf8_pos = 0;
  for (int i = 0; i < code_points; i++) {
    UtfCodepoint codepoint =
        index_bit_array_value(ba, bit_offset + i * 32, 32, endian).u32;

    // Encode to UTF-8
    if (codepoint <= 0x7F) {
      result.bytes[utf8_pos++] = codepoint;
    } else if (codepoint <= 0x7FF) {
      result.bytes[utf8_pos++] = 0xC0 | (codepoint >> 6);
      result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
    } else if (codepoint <= 0xFFFF) {
      result.bytes[utf8_pos++] = 0xE0 | (codepoint >> 12);
      result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 6) & 0x3F);
      result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
    } else {
      result.bytes[utf8_pos++] = 0xF0 | (codepoint >> 18);
      result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 12) & 0x3F);
      result.bytes[utf8_pos++] = 0x80 | ((codepoint >> 6) & 0x3F);
      result.bytes[utf8_pos++] = 0x80 | (codepoint & 0x3F);
    }
  }

  result.byte_length = utf8_pos;
  return result;
}

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
  if (length < 0) {
    return new_String(in.bytes, 0);
  }
  if (offset < 0) {
    offset = 0;
  }
  if (offset + length > in.byte_length) {
    length = in.byte_length - offset;
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

Nil print_string(String a) {
  printf("%.*s", a.byte_length, a.bytes);
  return 0;
}

Nil print_string_error(String a) {
  fprintf(stderr, "%.*s", a.byte_length, a.bytes);
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
  char formatted[32];
  int buffer_size = sizeof(buffer);

  // find the smallest precision that can represent the
  // value exactly
  for (int precision = 3; precision <= DBL_DECIMAL_DIG; precision++) {
    snprintf(buffer, sizeof(buffer), "%.*g", precision, value);
    double parsed = strtod(buffer, NULL);
    if (parsed == value) {
      break;
    }
  }

  // Find 'e' position for potential scientific notation
  char *e_pos = strchr(buffer, 'e');

  if (e_pos) {
    // Copy base part
    char base[32];
    strncpy(base, buffer, e_pos - buffer);
    base[e_pos - buffer] = '\0';

    // Ensure base has decimal point
    if (strchr(base, '.') == NULL) {
      base[e_pos - buffer] = '.';
      base[e_pos - buffer + 1] = '0';
      base[e_pos - buffer + 2] = '\0';
    }

    // Remove leading zero from exponent
    char neg = e_pos[1] == '-';
    char *exp_num = e_pos + 2;

    // Remove leading zero from exponent
    while (*exp_num == '0' && *(exp_num + 1) != '\0') {
      exp_num++;
    }

    // Reconstruct the number
    char *format = neg ? "%se-%s" : "%se%s";
    snprintf(formatted, sizeof(formatted), format, base, exp_num);
  } else {
    // Non-scientific notation: ensure decimal point
    if (strchr(buffer, '.') == NULL) {
      snprintf(formatted, sizeof(formatted), "%s.0", buffer);
    } else {
      strcpy(formatted, buffer);
    }
  }

  return cstring_to_string(formatted);
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

  // Buffer size: 3 chars per byte (max 255), plus
  // commas, brackets, and null terminator
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

    byte = byte >> ((start_bit - end_bit + 8) % 8);

    // Convert byte to decimal string
    bp += sprintf(&buffer[bp], "%d", byte);
  }

  buffer[bp++] = '>';
  buffer[bp++] = '>';
  buffer[bp] = '\0';

  return cstring_to_string(buffer);
}

String inspect_Closure(Closure c) { return new_String("Closure", 7); }

// UTF string size calculation functions
Int utf8_string_bit_size(String value) {
  // UTF-8 strings are already encoded, so just return
  // the byte size in bits
  return 8 * value.byte_length;
}

Int utf16_string_bit_size(String value) {
  // Calculate actual UTF-16 encoded size in bits
  int byte_length = value.byte_length;
  int bit_size = 0;

  for (int i = 0; i < byte_length;) {
    uint8_t first_byte = value.bytes[i];
    UtfCodepoint codepoint;
    int bytes_used;

    if (first_byte < 0x80) {
      codepoint = first_byte;
      bytes_used = 1;
    } else if ((first_byte & 0xE0) == 0xC0) {
      codepoint = ((first_byte & 0x1F) << 6) | (value.bytes[i + 1] & 0x3F);
      bytes_used = 2;
    } else if ((first_byte & 0xF0) == 0xE0) {
      codepoint = ((first_byte & 0x0F) << 12) |
                  ((value.bytes[i + 1] & 0x3F) << 6) |
                  (value.bytes[i + 2] & 0x3F);
      bytes_used = 3;
    } else if ((first_byte & 0xF8) == 0xF0) {
      codepoint =
          ((first_byte & 0x07) << 18) | ((value.bytes[i + 1] & 0x3F) << 12) |
          ((value.bytes[i + 2] & 0x3F) << 6) | (value.bytes[i + 3] & 0x3F);
      bytes_used = 4;
    } else {
      // Invalid UTF-8, skip
      bytes_used = 1;
      i += bytes_used;
      continue;
    }

    if (codepoint <= 0xFFFF) {
      bit_size += 16; // Single code unit
    } else {
      bit_size += 32; // Surrogate pair
    }
    i += bytes_used;
  }

  return bit_size;
}

Int utf32_string_bit_size(String value) {
  // Calculate actual UTF-32 encoded size in bits
  int byte_length = value.byte_length;
  int codepoint_count = 0;

  for (int i = 0; i < byte_length;) {
    uint8_t first_byte = value.bytes[i];
    int bytes_used;

    if (first_byte < 0x80) {
      bytes_used = 1;
    } else if ((first_byte & 0xE0) == 0xC0) {
      bytes_used = 2;
    } else if ((first_byte & 0xF0) == 0xE0) {
      bytes_used = 3;
    } else if ((first_byte & 0xF8) == 0xF0) {
      bytes_used = 4;
    } else {
      bytes_used = 1;
    }

    codepoint_count++;
    i += bytes_used;
  }

  return codepoint_count * 32;
}

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
