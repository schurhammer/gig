#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/// builtin

#define True true
#define False false
#define Int int
#define Bool bool

int panic() { exit(1); }

int print_int(int number) {
  printf("%d\n", number);
  return number;
}

bool equal(int x, int y) { return x == y; }

int add_int(int x, int y) { return x + y; }

int sub_int(int x, int y) { return x - y; }

int mul_int(int x, int y) { return x * y; }

int div_int(int x, int y) { return x / y; }

typedef struct Closure Closure;

struct Closure {
  void *fun;
  void *env;
};

Closure create_closure(void *fun, void *env) {
  Closure RETURN;
  RETURN.fun = fun;
  RETURN.env = env;
  return RETURN;
}

/// end of builtin

/// codegen

///CODEGEN_CONTENT///

/// end of codegen

/// main

int main() { print_int(main_T_()); }