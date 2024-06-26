#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/// INCLUDES

#ifdef GC

struct Pointer
{
  uint16_t tag;
  void *ptr;
};

typedef struct Pointer Pointer;

Pointer encode_pointer(void *ptr, uint16_t tag)
{
  Pointer point;
  point.tag = tag;
  point.ptr = ptr;
  return point;
}

void *decode_pointer(Pointer ptr)
{
  return ptr.ptr;
}

uint16_t decode_tag(Pointer ptr)
{
  return ptr.tag;
}

// TODO figure out how to use POINTER_MASK so we don't need to use fat pointers
// or implement a custom GC
#include <gc.h>
#define malloc(x) GC_MALLOC(x)

#else

typedef uintptr_t Pointer;

Pointer encode_pointer(void *ptr, uint16_t tag)
{
  Pointer tagged_ptr = ((uintptr_t)ptr & 0x0000FFFFFFFFFFFF) | ((uintptr_t)tag << 48);
  return tagged_ptr;
}

void *decode_pointer(Pointer ptr)
{
  return (void *)(ptr & 0x0000FFFFFFFFFFFF);
}

uint16_t decode_tag(Pointer ptr)
{
  return (uint16_t)(ptr >> 48);
}

#define INITIAL_ARENA_SIZE 8 * 1024 * 1024 // 8 MB
#define ALIGNMENT 8

typedef struct Arena
{
  size_t size;
  size_t used;
  struct Arena *next;
  char data[];
} Arena;

static Arena *current_arena = NULL;

static size_t align_up(size_t size, size_t alignment)
{
  return (size + alignment - 1) & ~(alignment - 1);
}

void *arena_malloc(size_t size)
{
  size = align_up(size, ALIGNMENT);

  if (!current_arena || current_arena->used + size > current_arena->size)
  {
    size_t arena_size = INITIAL_ARENA_SIZE > size ? INITIAL_ARENA_SIZE : size + sizeof(Arena);
    Arena *new_arena = (Arena *)malloc(sizeof(Arena) + arena_size);

    if (!new_arena)
    {
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

#define True true
#define False false
#define Int int64_t
#define Float double
#define Bool bool

typedef struct String String;
typedef struct Closure Closure;

// NOTE: Strings might not be null terminated
struct String
{
  int byte_length;
  char *bytes;
};

struct Closure
{
  void *fun;
  Pointer env;
};

// these must be short-circuiting
#define and_bool(x, y) (x && y)
#define or_bool(x, y) (x || y)

void panic_exit() { exit(1); }

Bool equal_Bool(Bool x, Bool y) { return x == y; }
Bool negate_bool(Bool x) { return !x; }
Bool isa_True(Bool x) { return x == True; }
Bool isa_False(Bool x) { return x == False; }

Bool equal_Int(Int x, Int y) { return x == y; }
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

Bool equal_Float(Float x, Float y) { return x == y; }
Bool lt_float(Float x, Float y) { return x < y; }
Bool gt_float(Float x, Float y) { return x > y; }
Bool lte_float(Float x, Float y) { return x <= y; }
Bool gte_float(Float x, Float y) { return x >= y; }
Float add_float(Float x, Float y) { return x + y; }
Float sub_float(Float x, Float y) { return x - y; }
Float mul_float(Float x, Float y) { return x * y; }
Float div_float(Float x, Float y) { return x / y; }

String String_LIT(char *bytes, int byte_length)
{
  if (byte_length < 0)
  {
    byte_length = strlen(bytes);
  }
  String str;
  str.byte_length = byte_length;
  str.bytes = bytes;

  return str;
}

Bool equal_String(String a, String b)
{
  if (a.byte_length != b.byte_length)
  {
    return False;
  }
  return strncmp(a.bytes, b.bytes, a.byte_length) == 0;
}

String append_String(String a, String b)
{
  if (a.byte_length == 0)
  {
    return b;
  }
  if (b.byte_length == 0)
  {
    return a;
  }
  String str;
  int byte_length = a.byte_length + b.byte_length;
  str.bytes = malloc(byte_length);
  str.byte_length = byte_length;
  memcpy(str.bytes, a.bytes, a.byte_length);
  memcpy(str.bytes + a.byte_length, b.bytes, b.byte_length);
  return str;
}

Bool starts_with_String(String string, String with)
{
  if (string.byte_length < with.byte_length)
  {
    return False;
  }
  for (int i = 0; i < with.byte_length; i++)
  {
    if (string.bytes[i] != with.bytes[i])
    {
      return False;
    }
  }
  return True;
}

Bool ends_with_String(String string, String with)
{
  if (string.byte_length < with.byte_length)
  {
    return False;
  }
  int j = string.byte_length;
  int k = with.byte_length;
  for (int i = 1; i <= with.byte_length; i++)
  {
    if (string.bytes[j - i] != with.bytes[k - i])
    {
      return False;
    }
  }
  return True;
}

Int compare_String(struct String str1, struct String str2) {
    if (str1.byte_length != str2.byte_length) {
        return str1.byte_length - str2.byte_length;
    }
    return memcmp(str1.bytes, str2.bytes, str1.byte_length);
}

String cstring_to_String(char *bytes)
{
  String str;
  str.byte_length = strlen(bytes);
  str.bytes = malloc(str.byte_length);
  memcpy(str.bytes, bytes, str.byte_length);
  return str;
}

Int print_String(String a)
{
  printf("%.*s", a.byte_length, a.bytes);
  return 0;
}

String gets_String(Int max_length)
{
  // extra space for null terminator
  max_length += 1;
  char buffer[max_length];
  if (fgets(buffer, max_length, stdin) != NULL)
  {
    return cstring_to_String(buffer);
  }
  else
  {
    struct String str;
    str.byte_length = 0;
    str.bytes = NULL;
    return str;
  }
}

String gleam_inspect_Bool(Bool b)
{
  if (b)
    return String_LIT("True", 4);
  else
    return String_LIT("False", 5);
}

String gleam_inspect_Int(Int value)
{
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%ld", value);

  struct String result;
  result.byte_length = strlen(buffer);
  result.bytes = malloc(result.byte_length);

  memcpy(result.bytes, buffer, result.byte_length);

  return result;
}

String gleam_inspect_Float(Float value)
{
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%g", value);

  struct String result;
  result.byte_length = strlen(buffer);
  result.bytes = malloc(result.byte_length);

  memcpy(result.bytes, buffer, result.byte_length);

  return result;
}

String gleam_inspect_String(String s)
{
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
  for (size_t sp = 0; sp < s.byte_length; sp++)
  {
    if (ooh[s.bytes[sp]])
    {
      buffer[bp++] = '\\';
      buffer[bp++] = ooh[s.bytes[sp]];
    }
    else
    {
      buffer[bp++] = s.bytes[sp];
    }
  }
  buffer[bp++] = '"';
  buffer[bp] = 0;
  return cstring_to_String(buffer);
}

String gleam_inspect_Closure(Closure c)
{
  return String_LIT("Closure", 7);
}

Closure create_closure(void *fun, Pointer env)
{
  Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = env;
  return RETURN;
}

Closure create_function(void *fun)
{
  Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = encode_pointer(0, 65535);
  return RETURN;
}

Bool is_closure(Closure c)
{
  return decode_tag(c.env) != 65535;
}

Bool equal_Closure(Closure a, Closure b)
{
  return False;
}

/// end of builtin

/// codegen

/// CODEGEN

/// end of codegen

/// main

int main()
{
#ifdef GC
  GC_INIT();
#endif
  /// INIT
  return 0;
}
