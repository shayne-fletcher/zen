#!/usr/bin/env bash

# Build a cabal project composed from a set of .tar.gz sdists of
# ghc-lib-parser, ghc-lib, ghc-lib-parser-ex and hlint. Choice of
# ghc-lib version & build compiler are provided as arguments. The name
# of the script is historical.

set -exo pipefail

prog=$(basename "$0")
opt_args="
opts:
    --ghc-lib-dir=ARG
    --ghc-lib-parser-ex-dir=ARG
    --hlint-dir=ARG
    --build-dir=ARG"
usage="usage: $prog --ghc-version=ARG --version-tag=ARG [opts]""

$opt_args"

[ ! -z "$1" ] && [[ "$1" =~ "--help" ]] && \
    echo "$usage" && exit 0
[ ! -z "$1" ] && [[ "$1" =~ --ghc-version=(.*)$ ]] &&  \
    ghc_version="${BASH_REMATCH[1]}" || \
        { echo "Missing ghc-version" && echo "$usage" && exit 1; }
[ ! -z "$2" ] && [[ "$2" =~ --version-tag=(.*)$ ]] && \
    version_tag="${BASH_REMATCH[1]}" || \
        { echo "Missing version-tag" && echo "$usage" && exit 1; }

[ ! -z "$3" ] && [[ "$3" =~ --ghc-lib-dir=(.*)$ ]] && ghc_lib_dir="${BASH_REMATCH[1]}"
[ -z "$ghc_lib_dir" ] && \
    ghc_lib_dir="$HOME/project/sf-ghc-lib" && \
    echo "Missing 'ghc-lib-dir': defaulting to $ghc_lib_dir"
[ ! -e  "$ghc_lib_dir" ] && { echo "\"$ghc_lib_dir\" does not exist" && exit 1;  }

[ ! -z "$4" ] && [[ "$4" =~ --ghc-lib-parser-ex-dir=(.*)$ ]] && ghc_lib_parser_ex_dir="${BASH_REMATCH[1]}"
[ -z "$ghc_lib_parser_ex_dir" ] && \
    ghc_lib_parser_ex_dir="$HOME/project/ghc-lib-parser-ex" && \
    echo "Missing 'ghc-lib-parser-ex-dir': defaulting to $ghc_lib_parser_ex_dir"
[ ! -e  "$ghc_lib_parser_ex_dir" ] && { echo "\"$ghc_lib_parser_ex_dir\" does not exist" && exit 1;  }

[ ! -z "$5" ] && [[ "$5" =~ --hlint-dir=(.*)$ ]] && hlint_dir="${BASH_REMATCH[1]}"
[ -z "$hlint_dir" ] && \
    hlint_dir="$HOME/project/hlint" && \
    echo "Missing 'hlint-dir': defaulting to $hlint_dir"
[ ! -e  "$hlint_dir" ] && { echo "\"$hlint_dir\" does not exist" && exit 1;  }

[ ! -z "$6" ] && [[ "$6" =~ --build-dir=(.*)$ ]] && build_dir="${BASH_REMATCH[1]}"
[ -z "$build_dir" ] && \
    build_dir="$HOME/tmp/ghc-lib/$version_tag" && \
    echo "Missing 'build-dir': defaulting to $build_dir"

set -u 

[[ ! -f "$HOME/$ghc_version/bin/ghc" ]] && \
    echo "$HOME/$ghc_version/bin/ghc not found" && \
    exit 2
PATH="$HOME/$ghc_version/bin:$PATH"
export PATH

echo "cabal-install: $(which cabal)"
echo "cabal-install version: $(cabal -V)"
echo "ghc: $(which ghc)"
echo "ghc version : $(ghc -V)"

rm -rf "$build_dir/$ghc_version"
mkdir -p "$build_dir/$ghc_version"
    
cd "$build_dir/$ghc_version"
cp "$ghc_lib_dir/ghc-lib-parser-$version_tag.tar.gz" .
cp "$ghc_lib_dir/ghc-lib-$version_tag.tar.gz" .
cp "$ghc_lib_parser_ex_dir/ghc-lib-parser-ex-$version_tag.tar.gz" .
cp "$hlint_dir/hlint-$version_tag.tar.gz" .
gunzip *.gz
for f in $(ls *.tar)
do
    tar xvf $f
    rm $f
done
cp -R "$ghc_lib_dir/examples/test-utils" "./test-utils-$version_tag"
cp -R "$ghc_lib_dir/examples/mini-hlint" "./mini-hlint-$version_tag"
cp -R "$ghc_lib_dir/examples/mini-compile" "./mini-compile-$version_tag"
cat > cabal.project<<EOF
packages:    */*.cabal
constraints: hlint +ghc-lib, ghc-lib-parser-ex -auto -no-ghc-lib
EOF

packages=("ghc-lib-parser-$version_tag" "ghc-lib-$version_tag" "ghc-lib-parser-ex-$version_tag" "mini-hlint-$version_tag" "mini-compile-$version_tag" "hlint-$version_tag")
for p in "${packages[@]}";
do
(cd "$p" && cabal check)
done

rm -rf dist-newstyle
flags="--ghc-option=-j"
cmd="cabal new-build all $flags"
ffi_inc_path="C_INCLUDE_PATH=$(xcrun --show-sdk-path)/usr/include/ffi"
ghc_version_number=$(ghc -V | tail -c 6)
[[ "$ghc_version_number" == "9.2.2" ]] && eval "$ffi_inc_path" "$cmd" ||  eval "$cmd"
    
cabal_project="$build_dir/$ghc_version/cabal.project"
project="--project-file $cabal_project"
run="cabal new-run exe"
(cd "mini-hlint-$version_tag" && eval "$run:mini-hlint" "$project" "--" "test/MiniHlintTest.hs")
(cd "mini-compile-$version_tag" && eval "$run:mini-compile" "$project" "--" "test/MiniCompileTest.hs" "|" "tail" "-10")
(cd "hlint-$version_tag" && eval "$run:hlint" "$project" "--" "--test")
