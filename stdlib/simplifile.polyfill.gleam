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
