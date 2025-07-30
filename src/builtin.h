#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifndef GLEAM_BUILTIN
#define GLEAM_BUILTIN

#ifdef GC
struct Pointer {
  uint16_t tag;
  void *ptr;
};
typedef struct Pointer Pointer;

#define encode_pointer(ptr, tag) ((Pointer){.ptr = (ptr), .tag = (tag)})
#define decode_pointer(ptr) ptr.ptr
#define decode_tag(ptr) ptr.tag

#else
typedef uintptr_t Pointer;

#define encode_pointer(ptr, tag)                                               \
  (((uintptr_t)ptr & 0x0000FFFFFFFFFFFF) | ((uintptr_t)tag << 48))
#define decode_pointer(ptr) (void *)(ptr & 0x0000FFFFFFFFFFFF)
#define decode_tag(ptr) (uint16_t)(ptr >> 48)

#endif

#define Nil int
#define True true
#define False false
#define Int int64_t
#define Float double
#define Bool bool
#define UtfCodepoint uint32_t

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

_Noreturn Nil panic_exit();

Bool eq_Nil(Nil x, Nil y);

Bool eq_Bool(Bool x, Bool y);
Bool and_bool(Bool x, Bool y);
Bool or_bool(Bool x, Bool y);
Bool negate_bool(Bool x);
Bool isa_True(Bool x);
Bool isa_False(Bool x);
Bool isa_Nil(Nil x);

Bool eq_Int(Int x, Int y);
Bool lt_int(Int x, Int y);
Bool gt_int(Int x, Int y);
Bool lte_int(Int x, Int y);
Bool gte_int(Int x, Int y);
Int add_int(Int x, Int y);
Int sub_int(Int x, Int y);
Int mul_int(Int x, Int y);
Int div_int(Int x, Int y);
Int rem_int(Int x, Int y);
Int negate_int(Int x);

Bool eq_Float(Float x, Float y);
Bool lt_float(Float x, Float y);
Bool gt_float(Float x, Float y);
Bool lte_float(Float x, Float y);
Bool gte_float(Float x, Float y);
Float add_float(Float x, Float y);
Float sub_float(Float x, Float y);
Float mul_float(Float x, Float y);
Float div_float(Float x, Float y);

Bool eq_UtfCodepoint(UtfCodepoint x, UtfCodepoint y);

BitArray new_bit_array(size_t len);
Nil write_bit_array(BitArray src, BitArray dst, Int offset, Int len);
Nil write_bit_array_string(String value, BitArray dst, Int offset, Int len);
Nil write_bit_array_int(Int value, BitArray dst, Int offset, Int len);
String index_bit_array_string(BitArray ba, Int bit_offset, Int bit_length);
Int index_bit_array_int(BitArray ba, Int bit_offset, Int bit_length);
BitArray slice_bit_array(BitArray ba, Int offset, Int len);
Int length_bit_array(BitArray ba);
Bool eq_BitArray(BitArray a, BitArray b);

u_int16_t splice_bits(u_int16_t src, u_int16_t dst, int src_offset,
                      int dst_offset, int n);

String String_LIT(char *bytes, int byte_length);
Bool eq_String(String a, String b);
Int length_string(String a);
String append_string(String a, String b);
Bool starts_with_string(String string, String with);
Bool ends_with_string(String string, String with);
String slice_string(String in, Int offset, Int length);
String drop_start_string(String string, Int count);
Int compare_string(String str1, String str2);
String cstring_to_string(char *bytes);
Int print_string(String a);
String gets_string(Int max_length);

String inspect_Nil(Nil value);
String inspect_Bool(Bool b);
String inspect_Int(Int value);
String inspect_Float(Float value);
String inspect_String(String s);
String inspect_BitArray(BitArray ba);
String inspect_Closure(Closure c);
String inspect_UtfCodepoint(UtfCodepoint value);

Closure create_closure(void *fun, Pointer env);
Closure create_function(void *fun);
Bool is_closure(Closure c);
Bool eq_Closure(Closure a, Closure b);
#endif
