#include "float.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Float gleam_float_exponential(Float a0) { return exp(a0); }

Float gleam_float_do_power(Float a0) { return log(a0); }

Float gleam_float_random() { return (Float)rand() / RAND_MAX; }

Float gleam_float_power(Float a0, Float a1) { return pow(a0, a1); }

Float gleam_float_do_to_float(Int a0) { return (Float)a0; }

Int gleam_float_truncate(Float a0) { return (Int)a0; }

Int gleam_float_js_round(Float a0) { return (Int)round(a0); }

Float gleam_float_floor(Float a0) { return floor(a0); }

Float gleam_float_ceiling(Float a0) { return ceil(a0); }

String gleam_float_to_string(Float a0) {
  char buffer[64];
  int len = snprintf(buffer, sizeof(buffer), "%g", a0);
  char *bytes = malloc(len + 1);
  if (bytes) {
    memcpy(bytes, buffer, len + 1);
  }
  return (String){.byte_length = len, .bytes = bytes};
}

Result_Float_Nil gleam_float_parse(String a0) {
  char *temp = malloc(a0.byte_length + 1);
  if (!temp) {
    Result_Float_Nil result = {.tag = Error_Float_Nil_TAG,
                               .val = {.Error = NULL}};
    return result;
  }
  memcpy(temp, a0.bytes, a0.byte_length);
  temp[a0.byte_length] = '\0';

  char *end;
  Float val = strtod(temp, &end);
  free(temp);

  if (end == temp) {
    // Parsing failed
    Result_Float_Nil result = {.tag = Error_Float_Nil_TAG,
                               .val = {.Error = NULL}};
    return result;
  }

  Result_Float_Nil result = {.tag = Ok_Float_Nil_TAG};
  struct Ok_Float_Nil *ok = malloc(sizeof(struct Ok_Float_Nil));
  ok->value = val;
  result.val.Ok = ok;
  return result;
}
