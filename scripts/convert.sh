#!/bin/sh

# tell iconv to convert your LATIN-1 file (`-fl1`) to utf-8 (`-t utf8`)
( echo "{-# LINE 1 \""$1"\" #-}" ; iconv -f l1 -t utf-8 "$1" ) > "$2"
cat "$2"
# the echo "{-# LINE 1 \""$1"\" #-}" just makes sure your error
# positions are reported as in the original source file.
