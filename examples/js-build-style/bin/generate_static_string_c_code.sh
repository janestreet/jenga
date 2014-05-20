#!/bin/bash

function_name=$1

stdin_as_hex_char_array() {
# Write a char array content, dumping each character from STDIN as a hex value
  while IFS= read -r -n 1 c
  do
    if [ "'$c" == "'" ]; then
      # Newline characters need special treatment because some special characters become
      # empty when "read". The test would be true as well for, e.g., null bytes in the input
      # but we assume that the input is a text file with only newline as the special char
      printf '0x0a, '
    else
      printf '0x%x, ' "'$c"
    fi
  done
  printf "0x00"
}

cat <<EOF
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value ${function_name} (value unit __attribute__ ((unused)))
{
  char v[] = { $(stdin_as_hex_char_array) };

  return(caml_copy_string(v));
}
EOF
