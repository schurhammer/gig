#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// INCLUDES

#ifdef GC

struct Pointer {
  uint16_t tag;
  void *ptr;
};

typedef struct Pointer Pointer;

#define encode_pointer(ptr, tag) ((Pointer){.ptr = (ptr), .tag = (tag)})

void *decode_pointer(Pointer ptr) { return ptr.ptr; }

uint16_t decode_tag(Pointer ptr) { return ptr.tag; }

// TODO figure out how to use POINTER_MASK so we don't need to use fat pointers
// or implement a custom GC
#include <gc.h>
#define malloc(x) GC_MALLOC(x)

#else

typedef uintptr_t Pointer;

#define encode_pointer(ptr, tag)                                               \
  (((uintptr_t)ptr & 0x0000FFFFFFFFFFFF) | ((uintptr_t)tag << 48))

void *decode_pointer(Pointer ptr) { return (void *)(ptr & 0x0000FFFFFFFFFFFF); }

uint16_t decode_tag(Pointer ptr) { return (uint16_t)(ptr >> 48); }

#define INITIAL_ARENA_SIZE 8 * 1024 * 1024 // 8 MB
#define ALIGNMENT 8

typedef struct Arena {
  size_t size;
  size_t used;
  struct Arena *next;
  char data[];
} Arena;

static Arena *current_arena = NULL;

static size_t align_up(size_t size, size_t alignment) {
  return (size + alignment - 1) & ~(alignment - 1);
}

void *arena_malloc(size_t size) {
  size = align_up(size, ALIGNMENT);

  if (!current_arena || current_arena->used + size > current_arena->size) {
    size_t arena_size =
        INITIAL_ARENA_SIZE > size ? INITIAL_ARENA_SIZE : size + sizeof(Arena);
    Arena *new_arena = (Arena *)malloc(sizeof(Arena) + arena_size);

    if (!new_arena) {
      return NULL; // Allocation failed
    }

    new_arena->size = arena_size;
    new_arena->used = 0;
    new_arena->next = current_arena;
    current_arena = new_arena;
  }

  void *ptr = current_arena->data + current_arena->used;
  current_arena->used += size;

  return ptr;
}

#define malloc(x) arena_malloc(x)

#endif

/// builtin

#define Nil int
#define True true
#define False false
#define Int int64_t
#define Float double
#define Bool bool

typedef struct String String;
typedef struct BitArray BitArray;
typedef struct Closure Closure;

struct String {
  int byte_length;
  char *bytes;
};

struct BitArray {
  uint8_t *bytes;
  size_t offset; // in bits
  size_t len;    // in bits
};

struct Closure {
  void *fun;
  Pointer env;
};

void panic_exit() { exit(1); }

#define eq_Nil(a, b) True

Bool eq_Bool(Bool x, Bool y) { return x == y; }
Bool and_bool(Bool x, Bool y) { return x && y; }
Bool or_bool(Bool x, Bool y) { return x || y; }
Bool negate_bool(Bool x) { return !x; }
Bool isa_True(Bool x) { return x == True; }
Bool isa_False(Bool x) { return x == False; }

Bool eq_Int(Int x, Int y) { return x == y; }
Bool lt_int(Int x, Int y) { return x < y; }
Bool gt_int(Int x, Int y) { return x > y; }
Bool lte_int(Int x, Int y) { return x <= y; }
Bool gte_int(Int x, Int y) { return x >= y; }
Int add_int(Int x, Int y) { return x + y; }
Int sub_int(Int x, Int y) { return x - y; }
Int mul_int(Int x, Int y) { return x * y; }
Int div_int(Int x, Int y) { return x / y; }
Int rem_int(Int x, Int y) { return x % y; }
Int negate_int(Int x) { return -x; }

Bool eq_Float(Float x, Float y) { return x == y; }
Bool lt_float(Float x, Float y) { return x < y; }
Bool gt_float(Float x, Float y) { return x > y; }
Bool lte_float(Float x, Float y) { return x <= y; }
Bool gte_float(Float x, Float y) { return x >= y; }
Float add_float(Float x, Float y) { return x + y; }
Float sub_float(Float x, Float y) { return x - y; }
Float mul_float(Float x, Float y) { return x * y; }
Float div_float(Float x, Float y) { return x / y; }

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
      a <<= 8;
      b <<= 8;
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

Nil write_bit_array_int(Int value, BitArray dst, Int offset, Int len) {
  // convert to big endian
  u_int8_t bytes[8];
  for (int i = 0; i < 8; i++) {
    bytes[i] = value >> (56 - 8 * i);
  }

  BitArray src;
  src.bytes = (uint8_t *)&bytes;
  src.offset = 64 - len;
  src.len = len;
  write_bit_array(src, dst, offset, len);
  return 0;
}

Int index_bit_array_int(BitArray ba, size_t bit_offset, int bit_length) {
  if (bit_length > 64)
    bit_length = 64; // Limit to 64 bits max

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

String String_LIT(char *bytes, int byte_length) {
  if (byte_length < 0) {
    byte_length = strlen(bytes);
  }
  struct String str;
  str.byte_length = byte_length;
  str.bytes = bytes;

  return str;
}

Bool eq_String(String a, String b) {
  if (a.byte_length != b.byte_length) {
    return False;
  }
  return strncmp(a.bytes, b.bytes, a.byte_length) == 0;
}

String append_string(String a, String b) {
  if (a.byte_length == 0) {
    return b;
  }
  if (b.byte_length == 0) {
    return a;
  }
  struct String str;
  int byte_length = a.byte_length + b.byte_length;
  str.bytes = malloc(byte_length);
  str.byte_length = byte_length;
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

Int compare_string(struct String str1, struct String str2) {
  if (str1.byte_length != str2.byte_length) {
    return str1.byte_length - str2.byte_length;
  }
  return memcmp(str1.bytes, str2.bytes, str1.byte_length);
}

String cstring_to_string(char *bytes) {
  struct String str;
  str.byte_length = strlen(bytes);
  str.bytes = malloc(str.byte_length);
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
    struct String str;
    str.byte_length = 0;
    str.bytes = NULL;
    return str;
  }
}

String inspect_Nil(Nil value) { return String_LIT("Nil", 3); }

String inspect_Bool(Bool b) {
  if (b)
    return String_LIT("True", 4);
  else
    return String_LIT("False", 5);
}

String inspect_Int(Int value) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%ld", value);

  struct String result;
  result.byte_length = strlen(buffer);
  result.bytes = malloc(result.byte_length);

  memcpy(result.bytes, buffer, result.byte_length);

  return result;
}

String inspect_Float(Float value) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%g", value);

  struct String result;
  result.byte_length = strlen(buffer);
  result.bytes = malloc(result.byte_length);

  memcpy(result.bytes, buffer, result.byte_length);

  return result;
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

  String result = cstring_to_string(buffer);
  return result;
}

String inspect_Closure(Closure c) { return String_LIT("Closure", 7); }

Closure create_closure(void *fun, Pointer env) {
  struct Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = env;
  return RETURN;
}

Closure create_function(void *fun) {
  struct Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = encode_pointer(0, 65535);
  return RETURN;
}

Bool is_closure(Closure c) { return decode_tag(c.env) != 65535; }

Bool eq_Closure(Closure a, Closure b) { return False; }

/// end of builtin

/// codegen

/// CODEGEN

/// end of codegen

/// main

int main() {
#ifdef GC
  GC_INIT();
#endif
  /// INIT
  return 0;
}
