#include "shellout.h"
#include <errno.h>
#include <stdio.h>
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
    current = current.ptr.v1->next;
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
    argv[i] = current.ptr.v1->item.bytes;
    current = current.ptr.v1->next;
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
    current = current.ptr.v1->next;
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

    Tuple2_String_String env_var = current.ptr.v1->item;
    String key = env_var.el0;
    String value = env_var.el1;

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
    current = current.ptr.v1->next;
  }
  envp[*envc] = NULL;

  return envp;
}

// Helper function to check if an option is set in the list
static Bool get_option_bool(List_Tuple2_shellout_CommandOpt_Bool opts,
                            shellout_CommandOpt opt) {
  List_Tuple2_shellout_CommandOpt_Bool current = opts;

  while (current.tag == Cons_Tuple2_shellout_CommandOpt_Bool_TAG) {
    Tuple2_shellout_CommandOpt_Bool pair = current.ptr.v1->item;
    shellout_CommandOpt current_opt = pair.el0;
    Bool value = pair.el1;

    // Check if this is the option we're looking for
    if (eq_shellout_CommandOpt(current_opt, opt)) {
      return value;
    }

    current = current.ptr.v1->next;
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
    free(argv);
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
    free(argv);
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

    // Clean up
    free(argv);
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
