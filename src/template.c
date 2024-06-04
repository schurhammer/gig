#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
///INCLUDES///

/// builtin

#define True true
#define False false
#define T_Int int64_t
#define T_Bool bool

// these must be short-circuiting
#define and_Bool(x, y) (x && y)
#define or_Bool(x, y) (x || y)

void panic() { exit(1); }

void print_int(T_Int number) {
  printf("%lld\n", number);
}

T_Bool equal_Bool(T_Bool x, T_Bool y) { return x == y; }
T_Bool negate_Bool(T_Bool x) { return  !x; }
T_Bool True_instanceof(T_Bool x) { return  x == True; }
T_Bool False_instanceof(T_Bool x) { return  x == False; }

T_Bool equal_Int(T_Int x, T_Int y) { return x == y; }

T_Bool lt_Int(T_Int x, T_Int y) { return x < y; }
T_Bool gt_Int(T_Int x, T_Int y) { return x > y; }
T_Bool lte_Int(T_Int x, T_Int y) { return x <= y; }
T_Bool gte_Int(T_Int x, T_Int y) { return x >= y; }

T_Int add_Int(T_Int x, T_Int y) { return x + y; }
T_Int sub_Int(T_Int x, T_Int y) { return x - y; }
T_Int mul_Int(T_Int x, T_Int y) { return x * y; }
T_Int div_Int(T_Int x, T_Int y) { return x / y; }

T_Int negate_Int(T_Int x) { return -x; }

typedef struct Closure T_Closure;
struct Closure {
  void *fun;
  void *env;
};

T_Closure create_closure(void *fun, void *env) {
  T_Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = env;
  return RETURN;
}

T_Bool equal_Closure(T_Closure a, T_Closure b) {
  return False;
}

/// end of builtin

/// codegen

///CODEGEN_CONTENT///

/// end of codegen

/// main

T_Int main() {
  ///INIT///
  print_int(F_main()); return 0;
}
