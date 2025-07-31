#include "simplifile.h"
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

// Helper function to convert errno to simplifile_FileError
static simplifile_FileError errno_to_file_error(int err) {
  switch (err) {
  case EACCES:
    return new_simplifile_Eacces;
  case EAGAIN:
    return new_simplifile_Eagain;
  case EBADF:
    return new_simplifile_Ebadf;
#ifdef EBADMSG
  case EBADMSG:
    return new_simplifile_Ebadmsg;
#endif
  case EBUSY:
    return new_simplifile_Ebusy;
#ifdef EDEADLK
  case EDEADLK:
    return new_simplifile_Edeadlk;
#endif
#ifdef EDEADLOCK
#if EDEADLOCK != EDEADLK
  case EDEADLOCK:
    return new_simplifile_Edeadlock;
#endif
#endif
#ifdef EDQUOT
  case EDQUOT:
    return new_simplifile_Edquot;
#endif
  case EEXIST:
    return new_simplifile_Eexist;
  case EFAULT:
    return new_simplifile_Efault;
  case EFBIG:
    return new_simplifile_Efbig;
#ifdef EFTYPE
  case EFTYPE:
    return new_simplifile_Eftype;
#endif
  case EINTR:
    return new_simplifile_Eintr;
  case EINVAL:
    return new_simplifile_Einval;
  case EIO:
    return new_simplifile_Eio;
  case EISDIR:
    return new_simplifile_Eisdir;
  case ELOOP:
    return new_simplifile_Eloop;
  case EMFILE:
    return new_simplifile_Emfile;
  case EMLINK:
    return new_simplifile_Emlink;
#ifdef EMULTIHOP
  case EMULTIHOP:
    return new_simplifile_Emultihop;
#endif
  case ENAMETOOLONG:
    return new_simplifile_Enametoolong;
  case ENFILE:
    return new_simplifile_Enfile;
#ifdef ENOBUFS
  case ENOBUFS:
    return new_simplifile_Enobufs;
#endif
  case ENODEV:
    return new_simplifile_Enodev;
#ifdef ENOLCK
  case ENOLCK:
    return new_simplifile_Enolck;
#endif
#ifdef ENOLINK
  case ENOLINK:
    return new_simplifile_Enolink;
#endif
  case ENOENT:
    return new_simplifile_Enoent;
  case ENOMEM:
    return new_simplifile_Enomem;
  case ENOSPC:
    return new_simplifile_Enospc;
#ifdef ENOSR
  case ENOSR:
    return new_simplifile_Enosr;
#endif
#ifdef ENOSTR
  case ENOSTR:
    return new_simplifile_Enostr;
#endif
  case ENOSYS:
    return new_simplifile_Enosys;
#ifdef ENOTBLK
  case ENOTBLK:
    return new_simplifile_Enotblk;
#endif
  case ENOTDIR:
    return new_simplifile_Enotdir;
#ifdef ENOTSUP
  case ENOTSUP:
    return new_simplifile_Enotsup;
#endif
  case ENXIO:
    return new_simplifile_Enxio;
#ifdef EOPNOTSUPP
#if EOPNOTSUPP != ENOTSUP
  case EOPNOTSUPP:
    return new_simplifile_Eopnotsupp;
#endif
#endif
  case EOVERFLOW:
    return new_simplifile_Eoverflow;
  case EPERM:
    return new_simplifile_Eperm;
  case EPIPE:
    return new_simplifile_Epipe;
  case ERANGE:
    return new_simplifile_Erange;
  case EROFS:
    return new_simplifile_Erofs;
  case ESPIPE:
    return new_simplifile_Espipe;
  case ESRCH:
    return new_simplifile_Esrch;
#ifdef ESTALE
  case ESTALE:
    return new_simplifile_Estale;
#endif
  case ETXTBSY:
    return new_simplifile_Etxtbsy;
  case EXDEV:
    return new_simplifile_Exdev;
  default: {
    // Create error string for unknown errno
    char error_msg[256];
    snprintf(error_msg, sizeof(error_msg), "Unknown error: %d", err);
    String error_string = cstring_to_string(error_msg);
    return new_simplifile_Unknown(error_string);
  }
  }
}

Result_List_String_simplifile_FileError simplifile_read_directory(String path) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_List_String_simplifile_FileError(new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Open directory
  DIR *dir = opendir(c_path);
  free(c_path);

  if (!dir) {
    int err = errno;
    return new_Error_List_String_simplifile_FileError(errno_to_file_error(err));
  }

  // Read directory entries and build list
  List_String result = new_Empty_String;
  struct dirent *entry;

  // Reset errno before reading
  errno = 0;

  while ((entry = readdir(dir)) != NULL) {
    // Skip "." and ".." entries
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
      continue;
    }

    // Convert filename to Gleam String
    String filename = cstring_to_string(entry->d_name);

    // Prepend to list (we'll reverse it later)
    result = new_Cons_String(filename, result);

    // Reset errno for next iteration
    errno = 0;
  }

  // Check if readdir failed
  if (errno != 0) {
    int err = errno;
    closedir(dir);
    return new_Error_List_String_simplifile_FileError(errno_to_file_error(err));
  }

  closedir(dir);

  // Reverse the list to maintain proper order
  List_String reversed = new_Empty_String;
  while (decode_tag(result) != 0) { // Not Empty_String
    struct Cons_String *cons = (struct Cons_String *)decode_pointer(result);
    String head = cons->f0;
    List_String tail = cons->f1;

    reversed = new_Cons_String(head, reversed);
    result = tail;
  }

  return new_Ok_List_String_simplifile_FileError(reversed);
}

Result_Bool_simplifile_FileError simplifile_is_directory(String path) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_Bool_simplifile_FileError(new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Get file statistics
  struct stat path_stat;
  int result = stat(c_path, &path_stat);
  free(c_path);

  if (result != 0) {
    int err = errno;
    return new_Error_Bool_simplifile_FileError(errno_to_file_error(err));
  }

  // Check if it's a directory
  Bool is_dir = S_ISDIR(path_stat.st_mode) ? True : False;
  return new_Ok_Bool_simplifile_FileError(is_dir);
}

Result_Bool_simplifile_FileError simplifile_is_file(String path) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_Bool_simplifile_FileError(new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Get file statistics
  struct stat path_stat;
  int result = stat(c_path, &path_stat);
  free(c_path);

  if (result != 0) {
    int err = errno;
    return new_Error_Bool_simplifile_FileError(errno_to_file_error(err));
  }

  // Check if it's a regular file
  Bool is_file = S_ISREG(path_stat.st_mode) ? True : False;
  return new_Ok_Bool_simplifile_FileError(is_file);
}

Result_simplifile_FileInfo_simplifile_FileError
simplifile_file_info(String path) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_simplifile_FileInfo_simplifile_FileError(
        new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Get file statistics
  struct stat path_stat;
  int result = stat(c_path, &path_stat);
  free(c_path);

  if (result != 0) {
    int err = errno;
    return new_Error_simplifile_FileInfo_simplifile_FileError(
        errno_to_file_error(err));
  }

  // Create FileInfo with stat fields
  // f0: size, f1: mode, f2: nlink, f3: inode, f4: user_id, f5: group_id
  // f6: dev, f7: atime, f8: mtime, f9: ctime
  simplifile_FileInfo file_info = new_simplifile_FileInfo(
      (Int)path_stat.st_size,  // f0: file size
      (Int)path_stat.st_mode,  // f1: file mode/permissions
      (Int)path_stat.st_nlink, // f2: number of hard links
      (Int)path_stat.st_ino,   // f3: inode number
      (Int)path_stat.st_uid,   // f4: user ID
      (Int)path_stat.st_gid,   // f5: group ID
      (Int)path_stat.st_dev,   // f6: device ID
      (Int)path_stat.st_atime, // f7: access time
      (Int)path_stat.st_mtime, // f8: modification time
      (Int)path_stat.st_ctime  // f9: status change time
  );

  return new_Ok_simplifile_FileInfo_simplifile_FileError(file_info);
}

Result_BitArray_simplifile_FileError simplifile_read_bits(String path) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_BitArray_simplifile_FileError(new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Open file for reading
  FILE *file = fopen(c_path, "rb");
  free(c_path);

  if (!file) {
    int err = errno;
    return new_Error_BitArray_simplifile_FileError(errno_to_file_error(err));
  }

  // Get file size
  if (fseek(file, 0, SEEK_END) != 0) {
    int err = errno;
    fclose(file);
    return new_Error_BitArray_simplifile_FileError(errno_to_file_error(err));
  }

  long file_size = ftell(file);
  if (file_size < 0) {
    int err = errno;
    fclose(file);
    return new_Error_BitArray_simplifile_FileError(errno_to_file_error(err));
  }

  if (fseek(file, 0, SEEK_SET) != 0) {
    int err = errno;
    fclose(file);
    return new_Error_BitArray_simplifile_FileError(errno_to_file_error(err));
  }

  // Create BitArray
  BitArray bit_array = new_bit_array(file_size * 8);
  if (!bit_array.bytes) {
    fclose(file);
    return new_Error_BitArray_simplifile_FileError(new_simplifile_Enomem);
  }

  // Read file contents
  size_t bytes_read = fread(bit_array.bytes, 1, file_size, file);
  fclose(file);

  if (bytes_read != (size_t)file_size) {
    free(bit_array.bytes);
    return new_Error_BitArray_simplifile_FileError(new_simplifile_Eio);
  }

  return new_Ok_BitArray_simplifile_FileError(bit_array);
}

Result_Nil_simplifile_FileError simplifile_write_bits(String path,
                                                      BitArray data) {
  // Convert Gleam String to C string
  char *c_path = malloc(path.byte_length + 1);
  if (!c_path) {
    return new_Error_Nil_simplifile_FileError(new_simplifile_Enomem);
  }

  memcpy(c_path, path.bytes, path.byte_length);
  c_path[path.byte_length] = '\0';

  // Open file for writing
  FILE *file = fopen(c_path, "wb");
  free(c_path);

  if (!file) {
    int err = errno;
    return new_Error_Nil_simplifile_FileError(errno_to_file_error(err));
  }

  // Calculate number of bytes to write
  size_t byte_length = (data.len + 7) / 8; // Round up to nearest byte

  // Write data
  size_t bytes_written = fwrite(data.bytes, 1, byte_length, file);
  fclose(file);

  if (bytes_written != byte_length) {
    return new_Error_Nil_simplifile_FileError(new_simplifile_Eio);
  }

  return new_Ok_Nil_simplifile_FileError(0); // Nil value
}
