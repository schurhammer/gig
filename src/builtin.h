#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifndef GLEAM_BUILTIN
#define GLEAM_BUILTIN

#ifdef GC

#include <gc.h>
#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)

#endif

extern int global_argc;
extern char **global_argv;

#define Nil int
#define True true
#define False false

typedef bool Bool;
typedef int64_t Int;
typedef double Float;
typedef struct String String;
typedef struct BitArray BitArray;
typedef uint32_t UtfCodepoint;
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
  void *env;
};

_Noreturn Nil panic_exit(String message);

Bool eq_Nil(Nil x, Nil y);
Bool eq_Bool(Bool x, Bool y);
Bool eq_Int(Int x, Int y);
Bool eq_Float(Float x, Float y);
Bool eq_UtfCodepoint(UtfCodepoint x, UtfCodepoint y);
Bool eq_BitArray(BitArray a, BitArray b);
Bool eq_String(String a, String b);
Bool eq_Closure(Closure a, Closure b);

Bool lt_Nil(Nil x, Nil y);
Bool lt_Bool(Bool x, Bool y);
Bool lt_UtfCodepoint(UtfCodepoint x, UtfCodepoint y);
Bool lt_BitArray(BitArray a, BitArray b);
Bool lt_String(String a, String b);
Bool lt_Closure(Closure a, Closure b);

Bool and_bool(Bool x, Bool y);
Bool or_bool(Bool x, Bool y);
Bool negate_bool(Bool x);

Bool lt_Int(Int x, Int y);
Bool gt_Int(Int x, Int y);
Bool lte_Int(Int x, Int y);
Bool gte_Int(Int x, Int y);
Int add_Int(Int x, Int y);
Int sub_Int(Int x, Int y);
Int mul_Int(Int x, Int y);
Int div_Int(Int x, Int y);
Int rem_Int(Int x, Int y);
Int negate_Int(Int x);

Bool lt_Float(Float x, Float y);
Bool gt_Float(Float x, Float y);
Bool lte_Float(Float x, Float y);
Bool gte_Float(Float x, Float y);
Float add_Float(Float x, Float y);
Float sub_Float(Float x, Float y);
Float mul_Float(Float x, Float y);
Float div_Float(Float x, Float y);

BitArray new_bit_array(size_t len);
Nil write_bit_array(BitArray src, BitArray dst, Int offset, Int len);
Nil write_bit_array_string(String value, BitArray dst, Int offset, Int len);
Nil write_bit_array_int(Int value, BitArray dst, Int offset, Int len);
String index_bit_array_string(BitArray ba, Int bit_offset, Int bit_length);
Int index_bit_array_int(BitArray ba, Int bit_offset, Int bit_length);
BitArray slice_bit_array(BitArray ba, Int offset, Int len);
Int length_bit_array(BitArray ba);

u_int16_t splice_bits(u_int16_t src, u_int16_t dst, int src_offset,
                      int dst_offset, int n);

String new_String(char *bytes, int byte_length);
Int length_string(String a);
String append_string(String a, String b);
Bool starts_with_string(String string, String with);
Bool ends_with_string(String string, String with);
String slice_string(String in, Int offset, Int length);
String drop_start_string(String string, Int count);
Int compare_string(String str1, String str2);
String cstring_to_string(char *bytes);
Int print_string(String a);
Int print_string_error(String a);

String inspect_Nil(Nil value);
String inspect_Bool(Bool b);
String inspect_Int(Int value);
String inspect_Float(Float value);
String inspect_String(String s);
String inspect_BitArray(BitArray ba);
String inspect_Closure(Closure c);
String inspect_UtfCodepoint(UtfCodepoint value);

Closure create_closure(void *fun, void *env);
Closure create_function(void *fun);
Bool is_closure(Closure c);

void init(int argc, char **argv);
#endif
