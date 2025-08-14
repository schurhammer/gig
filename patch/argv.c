#include "argv.h"

static List_String build_string_list(int start_index) {
  if (start_index >= global_argc) {
    return new_Empty_String;
  }

  // Build list in reverse order to maintain correct order
  List_String result = new_Empty_String;

  for (int i = global_argc - 1; i >= start_index; i--) {
    String arg = cstring_to_string(global_argv[i]);
    result = new_Cons_String(arg, result);
  }

  return result;
}

Tuple3_String_String_List_String argv_do() {
  String executable_path;
  String script_path;
  List_String remaining_args;

  if (global_argc >= 1) {
    executable_path = cstring_to_string(global_argv[0]);
  } else {
    executable_path = new_String("<unknown>", 9);
  }

  // no script path since this is a binary
  script_path = executable_path;

  if (global_argc >= 2) {
    remaining_args = build_string_list(1);
  } else {
    remaining_args = new_Empty_String;
  }

  return new_Tuple3_String_String_List_String(executable_path, script_path,
                                              remaining_args);
}
