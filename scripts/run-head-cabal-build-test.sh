#!/usr/bin/env bash

# Build a cabal project composed from a set of .tar.gz sdists of
# ghc-lib-parser, ghc-lib, ghc-lib-parser-ex and hlint. Choice of
# ghc-lib version & build compiler are provided as arguments.
#
# This script relies on
#  -  $HOME/$ghc_version/bin
#  -  /Users/shayne/.cabal/bin being in PATH

set -exo pipefail

prog=$(basename "$0")
opt_args="
opts:
    --ghc-lib-dir=ARG
    --ghc-lib-parser-ex-dir=ARG
    --hlint-dir=ARG
    --build-dir=ARG
    --with-haddock"
usage="usage: $prog --ghc-version=ARG --version-tag=ARG [opts]""

$opt_args"

[ -n "$1" ] && [[ "$1" =~ "--help" ]] && echo "$usage" && exit 0
if [ -n "$1" ] && [[ "$1" =~ --ghc-version=(.*)$ ]]; then
    ghc_version="${BASH_REMATCH[1]}"
else
    echo "Missing ghc-version" && echo "$usage" && exit 1
fi
if [ -n "$2" ] && [[ "$2" =~ --version-tag=(.*)$ ]]; then
   version_tag="${BASH_REMATCH[1]}"
else
    echo "Missing version-tag" && echo "$usage" && exit 1
fi
[ -n "$3" ] && [[ "$3" =~ --ghc-lib-dir=(.*)$ ]] && ghc_lib_dir="${BASH_REMATCH[1]}"
[ -z "$ghc_lib_dir" ] && \
    ghc_lib_dir="$HOME/project/sf-ghc-lib" && \
    echo "Missing 'ghc-lib-dir': defaulting to $ghc_lib_dir"
[ ! -e  "$ghc_lib_dir" ] && { echo "\"$ghc_lib_dir\" does not exist" && exit 1;  }

[ -n "$4" ] && [[ "$4" =~ --ghc-lib-parser-ex-dir=(.*)$ ]] && ghc_lib_parser_ex_dir="${BASH_REMATCH[1]}"
[ -z "$ghc_lib_parser_ex_dir" ] && \
    ghc_lib_parser_ex_dir="$HOME/project/ghc-lib-parser-ex" && \
    echo "Missing 'ghc-lib-parser-ex-dir': defaulting to $ghc_lib_parser_ex_dir"
[ ! -e  "$ghc_lib_parser_ex_dir" ] && { echo "\"$ghc_lib_parser_ex_dir\" does not exist" && exit 1;  }

[ -n "$5" ] && [[ "$5" =~ --hlint-dir=(.*)$ ]] && hlint_dir="${BASH_REMATCH[1]}"
[ -z "$hlint_dir" ] && \
    hlint_dir="$HOME/project/hlint" && \
    echo "Missing 'hlint-dir': defaulting to $hlint_dir"
[ ! -e  "$hlint_dir" ] && { echo "\"$hlint_dir\" does not exist" && exit 1;  }

[ -n "$6" ] && [[ "$6" =~ --build-dir=(.*)$ ]] && build_dir="${BASH_REMATCH[1]}"
[ -z "$build_dir" ] && \
    build_dir="$HOME/tmp/ghc-lib/$version_tag" && \
    echo "Missing 'build-dir': defaulting to $build_dir"

with_haddock=false
[ -n "$7" ] && [[ "$7" =~ --with-haddock$ ]] && with_haddock=true

set -u

[[ ! -f "$HOME/$ghc_version/bin/ghc" ]] && { echo "$HOME/$ghc_version/bin/ghc not found" && exit 1; }
PATH="$HOME/$ghc_version/bin:$PATH"
export PATH

# Make sure cabal-install is up-to-date with the most recent
# available. At this time there aren't build plans for compilers >
# ghc-9.2.4.
(PATH=$HOME/ghc-9.2.4/bin:$PATH; export PATH && \
     cabal update && \
     cabal new-install cabal-install --overwrite-policy=always \
)

echo "cabal-install: $(which cabal)"
echo "cabal-install version: $(cabal -V)"
echo "ghc: $(which ghc)"
echo "ghc version : $(ghc -V)"

build_dir_for_this_ghc="$build_dir/$ghc_version"
mkdir -p "$build_dir_for_this_ghc"
cd "$build_dir_for_this_ghc"
packages=(                                                      \
 "$ghc_lib_dir/ghc-lib-gen-$version_tag.tar.gz"                 \
 "$ghc_lib_dir/ghc-lib-parser-$version_tag.tar.gz"              \
 "$ghc_lib_dir/ghc-lib-$version_tag.tar.gz"                     \
 "$ghc_lib_dir/ghc-lib-test-utils-$version_tag.tar.gz"          \
 "$ghc_lib_dir/ghc-lib-test-mini-hlint-$version_tag.tar.gz"     \
 "$ghc_lib_dir/ghc-lib-test-mini-compile-$version_tag.tar.gz"   \
 "$ghc_lib_parser_ex_dir/ghc-lib-parser-ex-$version_tag.tar.gz" \
 "$hlint_dir/hlint-$version_tag.tar.gz"                         \
)
set +e # remember to remove this later
for f in "${packages[@]}"; do
  tar xvf "$f"
  base=$(basename "$f")
  (cd "${base%.tar.gz}" && cabal check)
done
set -e

haddock=""
if [ "$with_haddock" ]; then
  DOLLAR="$"
  pkg="pkg"
  # shellcheck disable=SC2154
  haddock="haddock-all: true
haddock-hyperlink-source: true
haddock-executables: true
haddock-html-location: http://hackage.haskell.org/packages/archive/$DOLLAR$pkg/latest/doc/html
"
fi

# Requires cabal-instal >= 3.8.1.0
# (reference https://cabal.readthedocs.io/en/3.8/index.html)
cat > cabal.project<<EOF
packages:    */*.cabal
constraints: hlint +ghc-lib, ghc-lib-parser-ex -auto -no-ghc-lib
$haddock
EOF

# clean
cabal new-clean

# cabal new-build all
flags="--ghc-option=-j"
cmd="cabal new-build all $flags"
ffi_inc_path="C_INCLUDE_PATH=$(xcrun --show-sdk-path)/usr/include/ffi"
ghc_version_number=$(ghc -V | tail -c 6)
if [[ "$ghc_version_number" == "9.2.2" ]]; then
    eval "$ffi_inc_path" "$cmd"
else
    eval "$cmd"
fi

# cabal new-haddock all
if "$with_haddock"; then
  eval "cabal" "new-haddock" "all"
fi

# run tests
cabal_project="$build_dir_for_this_ghc/cabal.project"
project="--project-file $cabal_project"
run="cabal new-run exe"
(cd "ghc-lib-test-mini-hlint-$version_tag" && eval "$run:ghc-lib-test-mini-hlint" "$project" "--" "test/MiniHlintTest.hs")
(cd "ghc-lib-test-mini-compile-$version_tag" && eval "$run:ghc-lib-test-mini-compile" "$project" "--" "test/MiniCompileTest.hs" "|" "tail" "-10")
(cd "hlint-$version_tag" && eval "$run:hlint" "$project" "--" "--test")

exit 0
