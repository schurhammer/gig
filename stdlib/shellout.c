#include "shellout.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

/*
 * Implementation of shellout_c_do_command function
 *
 * This function executes a shell command with the given arguments, working
 * directory, options, and environment variables. It returns either:
 * - Ok(output) if the command succeeds (exit code 0)
 * - Error(exit_code, output) if the command fails
 *
 * The implementation is based on the JavaScript reference in shellout_ffi.mjs
 * and uses fork/exec to run the command in a subprocess.
 *
 * Parameters:
 * - command: The command to execute
 * - args: List of command arguments
 * - dir: Working directory for the command
 * - opts: Command options (currently not fully implemented)
 * - env_list: Environment variables as key-value pairs
 */

// Helper function to convert List_String to char** array
static char **list_string_to_argv(List_String args, int *argc) {
  // Count arguments first
  *argc = 0;
  List_String current = args;
  while (current.tag == Cons_String_TAG) {
    (*argc)++;
    current = current.val.Cons->next;
  }

  // Allocate array for arguments + NULL terminator
  char **argv = malloc((*argc + 1) * sizeof(char *));
  if (!argv)
    return NULL;

  // Fill the array
  current = args;
  for (int i = 0; i < *argc; i++) {
    if (current.tag != Cons_String_TAG)
      break;

    // Create null-terminated string for each argument
    String arg_string = current.val.Cons->item;
    char *null_terminated_arg = malloc(arg_string.byte_length + 1);
    if (!null_terminated_arg) {
      // Cleanup previously allocated strings on error
      for (int j = 0; j < i; j++) {
        free(argv[j]);
      }
      free(argv);
      return NULL;
    }
    memcpy(null_terminated_arg, arg_string.bytes, arg_string.byte_length);
    null_terminated_arg[arg_string.byte_length] = '\0';

    argv[i] = null_terminated_arg;
    current = current.val.Cons->next;
  }
  argv[*argc] = NULL;

  return argv;
}

// Helper function to convert List_Tuple2_String_String to environment variables
// This extends the current environment rather than replacing it
static char **list_env_to_envp(List_Tuple2_String_String env_list, int *envc) {
  extern char **environ;

  // Count existing environment variables
  int existing_envc = 0;
  if (environ) {
    while (environ[existing_envc]) {
      existing_envc++;
    }
  }

  // Count new environment variables
  int new_envc = 0;
  List_Tuple2_String_String current = env_list;
  while (current.tag == Cons_Tuple2_String_String_TAG) {
    new_envc++;
    current = current.val.Cons->next;
  }

  *envc = existing_envc + new_envc;

  if (*envc == 0)
    return NULL;

  // Allocate array for all env vars + NULL terminator
  char **envp = malloc((*envc + 1) * sizeof(char *));
  if (!envp)
    return NULL;

  // Copy existing environment
  for (int i = 0; i < existing_envc; i++) {
    envp[i] = environ[i];
  }

  // Add new environment variables
  current = env_list;
  for (int i = 0; i < new_envc; i++) {
    if (current.tag != Cons_Tuple2_String_String_TAG)
      break;

    Tuple2_String_String env_var = current.val.Cons->item;
    String key = env_var.field0;
    String value = env_var.field1;

    // Allocate space for "KEY=VALUE"
    int len =
        key.byte_length + value.byte_length + 2; // +1 for '=', +1 for '\0'
    char *env_str = malloc(len);
    if (!env_str) {
      // Cleanup on error - only free the newly allocated strings
      for (int j = existing_envc; j < existing_envc + i; j++) {
        free(envp[j]);
      }
      free(envp);
      return NULL;
    }

    snprintf(env_str, len, "%.*s=%.*s", key.byte_length, key.bytes,
             value.byte_length, value.bytes);
    envp[existing_envc + i] = env_str;
    current = current.val.Cons->next;
  }
  envp[*envc] = NULL;

  return envp;
}

// Helper function to check if an option is set in the list
static Bool get_option_bool(List_Tuple2_shellout_CommandOpt_Bool opts,
                            shellout_CommandOpt opt) {
  List_Tuple2_shellout_CommandOpt_Bool current = opts;

  while (current.tag == Cons_Tuple2_shellout_CommandOpt_Bool_TAG) {
    Tuple2_shellout_CommandOpt_Bool pair = current.val.Cons->item;
    shellout_CommandOpt current_opt = pair.field0;
    Bool value = pair.field1;

    // Check if this is the option we're looking for
    if (eq_shellout_CommandOpt(current_opt, opt)) {
      return value;
    }

    current = current.val.Cons->next;
  }

  return False; // Default value if option not found
}

// Helper function to read all output from a file descriptor
static String read_fd_output(int fd) {
  char buffer[4096];
  size_t total_size = 0;
  char *output = NULL;
  ssize_t bytes_read;

  while ((bytes_read = read(fd, buffer, sizeof(buffer))) > 0) {
    char *new_output = realloc(output, total_size + bytes_read + 1);
    if (!new_output) {
      free(output);
      return new_String("", 0);
    }
    output = new_output;
    memcpy(output + total_size, buffer, bytes_read);
    total_size += bytes_read;
  }

  if (output) {
    output[total_size] = '\0';
    return new_String(output, total_size);
  } else {
    return new_String("", 0);
  }
}

Result_String_Tuple2_Int_String
shellout_c_do_command(String command, List_String args, String dir,
                      List_Tuple2_shellout_CommandOpt_Bool opts,
                      List_Tuple2_String_String env_list) {
  int argc;
  char **argv = list_string_to_argv(args, &argc);
  if (!argv && argc > 0) {
    Tuple2_Int_String error =
        new_Tuple2_Int_String(1, new_String("Memory allocation failed", 23));
    return new_Error_String_Tuple2_Int_String(error);
  }

  int envc;
  char **envp = list_env_to_envp(env_list, &envc);

  // Create pipes for stdout and stderr
  int stdout_pipe[2], stderr_pipe[2];
  if (pipe(stdout_pipe) == -1 || pipe(stderr_pipe) == -1) {
    // Clean up - free null-terminated strings we allocated in
    // list_string_to_argv
    if (argv) {
      for (int i = 0; i < argc; i++) {
        free(argv[i]);
      }
      free(argv);
    }
    if (envp) {
      for (int i = 0; i < envc; i++)
        free(envp[i]);
      free(envp);
    }
    Tuple2_Int_String error =
        new_Tuple2_Int_String(1, new_String("Failed to create pipes", 22));
    return new_Error_String_Tuple2_Int_String(error);
  }

  pid_t pid = fork();
  if (pid == -1) {
    // Fork failed
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    close(stderr_pipe[0]);
    close(stderr_pipe[1]);
    // Clean up - free null-terminated strings we allocated in
    // list_string_to_argv
    if (argv) {
      for (int i = 0; i < argc; i++) {
        free(argv[i]);
      }
      free(argv);
    }
    if (envp) {
      for (int i = 0; i < envc; i++)
        free(envp[i]);
      free(envp);
    }
    Tuple2_Int_String error =
        new_Tuple2_Int_String(1, new_String("Fork failed", 11));
    return new_Error_String_Tuple2_Int_String(error);
  }

  if (pid == 0) {
    // Child process

    // Change directory if specified
    if (dir.byte_length > 0) {
      char *dir_str = malloc(dir.byte_length + 1);
      if (dir_str) {
        memcpy(dir_str, dir.bytes, dir.byte_length);
        dir_str[dir.byte_length] = '\0';
        chdir(dir_str);
        free(dir_str);
      }
    }

    // Check options for stdio handling
    Bool let_be_stdout = get_option_bool(opts, new_shellout_LetBeStdout);
    Bool let_be_stderr = get_option_bool(opts, new_shellout_LetBeStderr);

    // Set up pipes
    close(stdout_pipe[0]); // Close read end
    close(stderr_pipe[0]); // Close read end

    if (!let_be_stdout) {
      dup2(stdout_pipe[1], STDOUT_FILENO); // Redirect stdout
    }
    if (!let_be_stderr) {
      dup2(stderr_pipe[1], STDERR_FILENO); // Redirect stderr
    }

    close(stdout_pipe[1]);
    close(stderr_pipe[1]);

    // Prepare command string
    char *cmd_str = malloc(command.byte_length + 1);
    if (cmd_str) {
      memcpy(cmd_str, command.bytes, command.byte_length);
      cmd_str[command.byte_length] = '\0';

      // Create full argv array with command as first argument
      char **full_argv = malloc((argc + 2) * sizeof(char *));
      if (full_argv) {
        full_argv[0] = cmd_str;
        for (int i = 0; i < argc; i++) {
          full_argv[i + 1] = argv[i];
        }
        full_argv[argc + 1] = NULL;

        // Execute command using execvp to search PATH
        if (envp) {
          // execvpe is not available on all systems, so we use execve with full
          // path For now, let's try execvp and set environment manually
          extern char **environ;
          char **old_environ = environ;
          environ = envp;
          execvp(cmd_str, full_argv);
          environ = old_environ; // Restore if exec fails
        } else {
          execvp(cmd_str, full_argv);
        }

        free(full_argv);
      }
      free(cmd_str);
    }

    // If we get here, exec failed
    _exit(127);
  } else {
    // Parent process
    close(stdout_pipe[1]); // Close write end
    close(stderr_pipe[1]); // Close write end

    // Read output from both pipes
    String stdout_output = read_fd_output(stdout_pipe[0]);
    String stderr_output = read_fd_output(stderr_pipe[0]);

    close(stdout_pipe[0]);
    close(stderr_pipe[0]);

    // Combine stdout and stderr
    String combined_output;
    if (stderr_output.byte_length > 0) {
      combined_output = append_string(stdout_output, stderr_output);
    } else {
      combined_output = stdout_output;
    }

    // Wait for child process
    int status;
    waitpid(pid, &status, 0);

    // Clean up - free null-terminated strings we allocated in
    // list_string_to_argv
    if (argv) {
      for (int i = 0; i < argc; i++) {
        free(argv[i]);
      }
      free(argv);
    }
    if (envp) {
      // Only free the newly allocated environment strings, not the copied ones
      extern char **environ;
      int existing_envc = 0;
      if (environ) {
        while (environ[existing_envc]) {
          existing_envc++;
        }
      }
      // Free only the strings we allocated (starting from existing_envc)
      for (int i = existing_envc; i < envc; i++)
        free(envp[i]);
      free(envp);
    }

    // Determine exit code
    int exit_code = 0;
    if (WIFEXITED(status)) {
      exit_code = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
      exit_code = 384 + WTERMSIG(status); // yash-like status
    }

    if (exit_code == 0) {
      return new_Ok_String_Tuple2_Int_String(combined_output);
    } else {
      Tuple2_Int_String error =
          new_Tuple2_Int_String(exit_code, combined_output);
      return new_Error_String_Tuple2_Int_String(error);
    }
  }
}

/*
 * Implementation of shellout_arguments function
 *
 * Returns the command line arguments passed to the program as a List_String.
 * Uses the global_argc and global_argv variables from builtin.h.
 */
List_String shellout_arguments() {
  List_String result = new_Empty_String;

  // Build list in reverse order, then we'll get it in correct order
  // Skip argv[0] (program name) and start from argv[1]
  for (int i = global_argc - 1; i >= 1; i--) {
    String arg = cstring_to_string(global_argv[i]);
    result = new_Cons_String(arg, result);
  }

  return result;
}

/*
 * Implementation of shellout_exit function
 *
 * Halts the runtime and passes the given status code to the operating system.
 * A status code of 0 typically indicates success, while any other integer
 * represents an error.
 */
Nil shellout_exit(Int status) {
  exit((int)status);
  return 0; // This will never be reached, but satisfies the return type
}

/*
 * Implementation of shellout_which function
 *
 * Finds the path to the given executable by searching the PATH environment
 * variable. Returns Ok(path) if found, Error(message) if not found.
 */
Result_String_String shellout_which(String executable) {
  // Convert Gleam String to C string
  char *exe_name = malloc(executable.byte_length + 1);
  if (!exe_name) {
    return new_Error_String_String(new_String("Memory allocation failed", 23));
  }
  memcpy(exe_name, executable.bytes, executable.byte_length);
  exe_name[executable.byte_length] = '\0';

  // If executable contains a path separator, check if it exists as-is
  if (strchr(exe_name, '/') != NULL) {
    if (access(exe_name, X_OK) == 0) {
      String result = new_String(exe_name, executable.byte_length);
      free(exe_name);
      return new_Ok_String_String(result);
    } else {
      free(exe_name);
      char error_msg[256];
      snprintf(error_msg, sizeof(error_msg), "command `%.*s` not found",
               executable.byte_length, executable.bytes);
      return new_Error_String_String(cstring_to_string(error_msg));
    }
  }

  // Get PATH environment variable
  char *path_env = getenv("PATH");
  if (!path_env) {
    free(exe_name);
    char error_msg[256];
    snprintf(error_msg, sizeof(error_msg), "command `%.*s` not found",
             executable.byte_length, executable.bytes);
    return new_Error_String_String(cstring_to_string(error_msg));
  }

  // Make a copy of PATH to tokenize
  char *path_copy = strdup(path_env);
  if (!path_copy) {
    free(exe_name);
    return new_Error_String_String(new_String("Memory allocation failed", 23));
  }

  char *dir = strtok(path_copy, ":");
  while (dir != NULL) {
    // Build full path: dir + "/" + exe_name
    size_t dir_len = strlen(dir);
    size_t full_path_len = dir_len + 1 + executable.byte_length + 1;
    char *full_path = malloc(full_path_len);
    if (!full_path) {
      free(exe_name);
      free(path_copy);
      return new_Error_String_String(
          new_String("Memory allocation failed", 23));
    }

    snprintf(full_path, full_path_len, "%s/%s", dir, exe_name);

    // Check if file exists and is executable
    if (access(full_path, X_OK) == 0) {
      String result = cstring_to_string(full_path);
      free(exe_name);
      free(path_copy);
      free(full_path);
      return new_Ok_String_String(result);
    }

    free(full_path);
    dir = strtok(NULL, ":");
  }

  // Not found in any PATH directory
  free(exe_name);
  free(path_copy);
  char error_msg[256];
  snprintf(error_msg, sizeof(error_msg), "command `%.*s` not found",
           executable.byte_length, executable.bytes);
  return new_Error_String_String(cstring_to_string(error_msg));
}
