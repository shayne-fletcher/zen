#!/usr/local/bin/bash

# The output of `opam env --sexp` as an associative array.

set -euo pipefail

input=$(opam env --sexp)

declare -A env
input="${input:1:-1}"
IFS=$'\n';set -f;lines=($input);set +f;IFS=$' \t\n'
for line in "${lines[@]}"; do
  # Remove parens
  line="${line//(/}"
  line="${line//)/}"
  # Remove leading & trailing white space
  line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
  # Split on space
  key="${line%% *}"
  value="${line#* }"
  # Remove quotes
  key="${key//\"/}"
  value="${value//\"/}"
  # Store the definition
  env[$key]=$value
done

echo "The prevailing switch is ${env["OPAM_SWITCH_PREFIX"]}"
