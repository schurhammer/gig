#include "string.h"

Result_Tuple2_String_String_Nil pop_grapheme_string(String str) {
  if (str.byte_length == 0) {
    return new_Error_Tuple2_String_String_Nil(0);
  }

  // Find the byte length of the first UTF-8 character
  unsigned char first_byte = (unsigned char)str.bytes[0];
  int grapheme_len;

  if ((first_byte & 0x80) == 0) {
    // ASCII: 0xxxxxxx
    grapheme_len = 1;
  } else if ((first_byte & 0xE0) == 0xC0) {
    // 2-byte: 110xxxxx
    grapheme_len = 2;
  } else if ((first_byte & 0xF0) == 0xE0) {
    // 3-byte: 1110xxxx
    grapheme_len = 3;
  } else if ((first_byte & 0xF8) == 0xF0) {
    // 4-byte: 11110xxx
    grapheme_len = 4;
  } else {
    // Invalid UTF-8, treat as single byte
    grapheme_len = 1;
  }

  // Ensure we don't read past the string end
  if (grapheme_len > str.byte_length) {
    grapheme_len = str.byte_length;
  }

  // Create the first grapheme string (zero-copy slice)
  String grapheme;
  grapheme.byte_length = grapheme_len;
  grapheme.bytes = str.bytes;

  // Create the rest string (zero-copy slice)
  String rest;
  rest.byte_length = str.byte_length - grapheme_len;
  rest.bytes = str.bytes + grapheme_len;

  // Create tuple #(grapheme, rest)
  Tuple2_String_String tuple = new_Tuple2_String_String(grapheme, rest);

  // Return Ok(tuple)
  return new_Ok_Tuple2_String_String_Nil(tuple);
}
