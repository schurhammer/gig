#include "string.h"
#include <stdlib.h>

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

UtfCodepoint gleam_string_unsafe_int_to_utf_codepoint(Int value) {
  // Unsafe conversion - no validation of the codepoint value
  return (UtfCodepoint)value;
}

String gleam_string_from_utf_codepoints(List_UtfCodepoint codepoints) {
  // First, count total bytes needed
  size_t total_bytes = 0;
  List_UtfCodepoint current = codepoints;

  while (current.tag == Cons_UtfCodepoint_TAG) {
    UtfCodepoint cp = current.ptr.Cons->item;

    // Calculate UTF-8 byte length for this codepoint
    if (cp <= 0x7F) {
      total_bytes += 1;
    } else if (cp <= 0x7FF) {
      total_bytes += 2;
    } else if (cp <= 0xFFFF) {
      total_bytes += 3;
    } else if (cp <= 0x10FFFF) {
      total_bytes += 4;
    }
    // Invalid codepoints (> 0x10FFFF) are ignored

    current = current.ptr.Cons->next;
  }

  if (total_bytes == 0) {
    return new_String("", 0);
  }

  // Allocate buffer
  char *bytes = malloc(total_bytes);
  if (!bytes) {
    return new_String("", 0);
  }

  // Fill buffer with UTF-8 encoded bytes
  size_t pos = 0;
  current = codepoints;

  while (current.tag == Cons_UtfCodepoint_TAG) {
    UtfCodepoint cp = current.ptr.Cons->item;

    if (cp <= 0x7F) {
      bytes[pos++] = (char)cp;
    } else if (cp <= 0x7FF) {
      bytes[pos++] = (char)(0xC0 | (cp >> 6));
      bytes[pos++] = (char)(0x80 | (cp & 0x3F));
    } else if (cp <= 0xFFFF) {
      bytes[pos++] = (char)(0xE0 | (cp >> 12));
      bytes[pos++] = (char)(0x80 | ((cp >> 6) & 0x3F));
      bytes[pos++] = (char)(0x80 | (cp & 0x3F));
    } else if (cp <= 0x10FFFF) {
      bytes[pos++] = (char)(0xF0 | (cp >> 18));
      bytes[pos++] = (char)(0x80 | ((cp >> 12) & 0x3F));
      bytes[pos++] = (char)(0x80 | ((cp >> 6) & 0x3F));
      bytes[pos++] = (char)(0x80 | (cp & 0x3F));
    }
    // Invalid codepoints (> 0x10FFFF) are silently skipped

    current = current.ptr.Cons->next;
  }

  return new_String(bytes, total_bytes);
}

List_UtfCodepoint gleam_string_do_to_utf_codepoints(String str) {
  if (str.byte_length == 0) {
    return new_Empty_UtfCodepoint;
  }

  // Build list in reverse order first
  List_UtfCodepoint temp_list = new_Empty_UtfCodepoint;

  int pos = 0;
  while (pos < str.byte_length) {
    unsigned char first_byte = (unsigned char)str.bytes[pos];
    UtfCodepoint codepoint;
    int char_len;

    if ((first_byte & 0x80) == 0) {
      // ASCII: 0xxxxxxx
      codepoint = first_byte;
      char_len = 1;
    } else if ((first_byte & 0xE0) == 0xC0) {
      // 2-byte: 110xxxxx 10xxxxxx
      if (pos + 1 < str.byte_length &&
          ((unsigned char)str.bytes[pos + 1] & 0xC0) == 0x80) {
        codepoint = ((first_byte & 0x1F) << 6) |
                    ((unsigned char)str.bytes[pos + 1] & 0x3F);
        char_len = 2;
      } else {
        codepoint = 0xFFFD; // Replacement character for invalid UTF-8
        char_len = 1;
      }
    } else if ((first_byte & 0xF0) == 0xE0) {
      // 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
      if (pos + 2 < str.byte_length &&
          ((unsigned char)str.bytes[pos + 1] & 0xC0) == 0x80 &&
          ((unsigned char)str.bytes[pos + 2] & 0xC0) == 0x80) {
        codepoint = ((first_byte & 0x0F) << 12) |
                    (((unsigned char)str.bytes[pos + 1] & 0x3F) << 6) |
                    ((unsigned char)str.bytes[pos + 2] & 0x3F);
        char_len = 3;
      } else {
        codepoint = 0xFFFD; // Replacement character for invalid UTF-8
        char_len = 1;
      }
    } else if ((first_byte & 0xF8) == 0xF0) {
      // 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      if (pos + 3 < str.byte_length &&
          ((unsigned char)str.bytes[pos + 1] & 0xC0) == 0x80 &&
          ((unsigned char)str.bytes[pos + 2] & 0xC0) == 0x80 &&
          ((unsigned char)str.bytes[pos + 3] & 0xC0) == 0x80) {
        codepoint = ((first_byte & 0x07) << 18) |
                    (((unsigned char)str.bytes[pos + 1] & 0x3F) << 12) |
                    (((unsigned char)str.bytes[pos + 2] & 0x3F) << 6) |
                    ((unsigned char)str.bytes[pos + 3] & 0x3F);
        char_len = 4;
      } else {
        codepoint = 0xFFFD; // Replacement character for invalid UTF-8
        char_len = 1;
      }
    } else {
      // Invalid UTF-8 start byte
      codepoint = 0xFFFD; // Replacement character
      char_len = 1;
    }

    // Prepend to temporary list (builds in reverse order)
    temp_list = new_Cons_UtfCodepoint(codepoint, temp_list);
    pos += char_len;
  }

  // Reverse the list to get correct order
  List_UtfCodepoint result = new_Empty_UtfCodepoint;
  while (temp_list.tag == Cons_UtfCodepoint_TAG) {
    UtfCodepoint item = temp_list.ptr.Cons->item;
    List_UtfCodepoint next = temp_list.ptr.Cons->next;
    result = new_Cons_UtfCodepoint(item, result);
    temp_list = next;
  }

  return result;
}
