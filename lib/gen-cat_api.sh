#!/bin/bash

# Embed the contents of the arguments files within an ML string bound
# to a variable named [string]. Write to stdout.

echo 'let string = "\' 
cat "$@" | sed 's/"/\\"/g' | sed '$s/$/"/' 
