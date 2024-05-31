#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

/// builtin

#define True true
#define False false
#define T_Int int64_t
#define T_Bool bool

T_Int panic() { exit(1); }

void print_int(T_Int number) {
  printf("%lld\n", number);
}

T_Bool equal(T_Int x, T_Int y) { return x == y; }

T_Int add_int(T_Int x, T_Int y) { return x + y; }

T_Int sub_int(T_Int x, T_Int y) { return x - y; }

T_Int mul_int(T_Int x, T_Int y) { return x * y; }

T_Int div_int(T_Int x, T_Int y) { return x / y; }

T_Bool and_bool(T_Bool x, T_Bool y) { return x && y; }

T_Bool or_bool(T_Bool x, T_Bool y) { return x && y; }

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

/// end of builtin

/// codegen

///CODEGEN_CONTENT///

/// end of codegen

/// main

T_Int main() { print_int(F_main()); return 0; }
