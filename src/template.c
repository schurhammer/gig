#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/// builtin

#define True true
#define False false
#define T_Int int
#define T_Bool bool

int panic() { exit(1); }

void print_int(int number) {
  printf("%d\n", number);
}

bool equal(int x, int y) { return x == y; }

int add_int(int x, int y) { return x + y; }

int sub_int(int x, int y) { return x - y; }

int mul_int(int x, int y) { return x * y; }

int div_int(int x, int y) { return x / y; }

bool and_bool(bool x, bool y) { return x && y; }

bool or_bool(bool x, bool y) { return x && y; }

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

int main() { print_int(F_main()); return 0; }
