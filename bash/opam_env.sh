#!/usr/local/bin/bash

# The output of `opam env --sexp` as an associative array.

set -euo pipefail

if [ ! "${BASH_VERSINFO:-0}" -ge 4 ]; then
    echo "require bash >= 4.0 for associative arrays"
    exit 2
fi

declare -A env

input=$(opam env --sexp)
# Remove parens
input="${input:1:-1}"
IFS=$'\n';set -f;lines=($input);set +f;IFS=$' \t\n'
for line in "${lines[@]}"; do
  # Remove leading & trailing white space
  line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
  # Remove parens
  line=${line:1:-1}
  # Split on space
  key="${line%% *}"
  value="${line#* }"
  # Remove quotes
  key=${key:1:-1}
  value=${value:1:-1}
  # Store the definition
  env[$key]=$value
done

echo "The prevailing switch is ${env["OPAM_SWITCH_PREFIX"]}"
