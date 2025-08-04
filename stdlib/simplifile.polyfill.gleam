@external(c, "", "simplifile_read_directory")
pub fn read_directory(at path: String) -> Result(List(String), FileError)

@external(c, "", "simplifile_is_directory")
pub fn is_directory(filepath: String) -> Result(Bool, FileError)

@external(c, "", "simplifile_is_file")
pub fn is_file(filepath: String) -> Result(Bool, FileError)

@external(c, "", "simplifile_file_info")
pub fn file_info(filepath: String) -> Result(FileInfo, FileError)

@external(c, "", "simplifile_read_bits")
pub fn read_bits(from filepath: String) -> Result(BitArray, FileError)

@external(c, "", "simplifile_write_bits")
pub fn write_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError)

@external(c, "", "simplifile_is_symlink")
pub fn is_symlink(filepath: String) -> Result(Bool, FileError)

@external(c, "", "simplifile_rename_file")
pub fn rename_file(at src: String, to dest: String) -> Result(Nil, FileError)

@external(c, "", "simplifile_erl_do_current_directory")
pub fn erl_do_current_directory() -> Result(List(UtfCodepoint), FileError)

@external(c, "", "simplifile_create_link")
pub fn create_link(
  to target: String,
  from link: String,
) -> Result(Nil, FileError)

@external(c, "", "simplifile_delete")
pub fn delete(file_or_dir_at path: String) -> Result(Nil, FileError)

@external(c, "", "simplifile_do_create_dir_all")
pub fn do_create_dir_all(dirpath: String) -> Result(Nil, FileError)

@external(c, "", "simplifile_append_bits")
pub fn append_bits(
  to filepath: String,
  bits bits: BitArray,
) -> Result(Nil, FileError)

@external(c, "", "simplifile_create_symlink")
pub fn create_symlink(
  to target: String,
  from symlink: String,
) -> Result(Nil, FileError)

@external(c, "", "simplifile_link_info")
pub fn link_info(filepath: String) -> Result(FileInfo, FileError)

@external(c, "", "simplifile_do_copy_file")
pub fn do_copy_file(src: String, dest: String) -> Result(Int, FileError)

@external(c, "", "simplifile_set_permissions_octal")
pub fn set_permissions_octal(
  for_file_at filepath: String,
  to permissions: Int,
) -> Result(Nil, FileError)

@external(c, "", "simplifile_rename")
pub fn rename(at src: String, to dest: String) -> Result(Nil, FileError)

@external(c, "", "simplifile_create_directory")
pub fn create_directory(filepath: String) -> Result(Nil, FileError)
