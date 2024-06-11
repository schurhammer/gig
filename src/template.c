#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

///INCLUDES///

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
#define Bool bool

// these must be short-circuiting
#define and_Bool(x, y) (x && y)
#define or_Bool(x, y) (x || y)

void panic() { exit(1); }

Bool equal_Bool(Bool x, Bool y) { return x == y; }
Bool negate_Bool(Bool x) { return !x; }
Bool isa_True(Bool x) { return x == True; }
Bool isa_False(Bool x) { return x == False; }

Bool equal_Int(Int x, Int y) { return x == y; }

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

typedef struct String String;

struct String
{
  uint64_t byte_length;
  uint8_t *bytes;
};

String String_NEW(uint8_t *bytes, int byte_length)
{
  // TODO handle empty strings?
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
  // TODO handle empty strings?
  String str;
  int byte_length = a.byte_length + b.byte_length;
  str.bytes = malloc(byte_length);
  str.byte_length = byte_length;
  memcpy(str.bytes, a.bytes, a.byte_length);
  memcpy(str.bytes + a.byte_length, b.bytes, b.byte_length);
  return str;
}

String inspect_String(String s)
{
  String q = String_NEW("\"", 1);
  append_String(q, append_String(s, q));
}

String inspect_Bool(Bool b)
{
  if (b)
  {
    return String_NEW("True", 4);
  }
  else
  {
    return String_NEW("False", 5);
  }
}

int countDigits(int64_t num)
{
  int count = 0;
  if (num == 0)
    return 1;
  while (num != 0)
  {
    num /= 10;
    count++;
  }
  return count;
}

String inspect_Int(Int num)
{
  int length = countDigits(num);
  if (num < 0)
  {
    length++;
  }

  struct String result;
  result.byte_length = length;
  result.bytes = (uint8_t *)malloc(length);

  if (num < 0)
  {
    result.bytes[0] = '-';
    num = -num;
  }

  int i = length - 1;
  do
  {
    result.bytes[i] = num % 10 + '0';
    i = i - 1;
    num /= 10;
  } while (num != 0);

  return result;
}

String print(String a)
{
  printf("%.*s", a.byte_length, a.bytes);
  return a;
}

typedef struct Closure Closure;

struct Closure
{
  void *fun;
  Pointer env;
};

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

String inspect_Closure(Closure c)
{
  return String_NEW("Closure", 7);
}

/// end of builtin

/// codegen

///CODEGEN_CONTENT///

/// end of codegen

/// main

Int main()
{
#ifdef GC
  GC_INIT();
#endif
  ///INIT///
  return 0;
}
