#!/usr/bin/env bash

# Build a cabal project composed from a set of .tar.gz sdists of
# ghc-lib-parser, ghc-lib, ghc-lib-parser-ex and hlint. Choice of
# ghc-lib version & build compiler are provided as arguments. The name
# of the script is historical.

set -exo pipefail

prog=$(basename "$0")
usage="usage: $prog --ghc-version=ARG --version-tag=ARG"

[ ! -z "$1" ] && [[ "$1" =~ "--help" ]] && \
    echo "usage: $usage" && exit 0
[ ! -z "$1" ] && [[ "$1" =~ --ghc-version=(.*)$ ]] &&  \
    ghc_version="${BASH_REMATCH[1]}" || \
        { echo "Missing ghc-version" && echo "$usage" && exit 1; }
[ ! -z "$2" ] && [[ "$2" =~ --version-tag=(.*)$ ]] && \
    version_tag="${BASH_REMATCH[1]}" || \
        { echo "Missing version-tag" && echo "$usage" && exit 1; }

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

ghc_lib_dir=~/project/sf-ghc-lib
ghc_lib_parser_ex_dir=~/project/ghc-lib-parser-ex
build_dir=~/tmp/ghc-lib/$version_tag

rm -rf $build_dir/$ghc_version
mkdir -p $build_dir/$ghc_version
cd $build_dir/$ghc_version
cp $ghc_lib_dir/ghc-lib-parser-$version_tag.tar.gz .
cp $ghc_lib_dir/ghc-lib-$version_tag.tar.gz .
cp $ghc_lib_parser_ex_dir/ghc-lib-parser-ex-$version_tag.tar.gz .
gunzip *.gz
for f in $(ls *.tar)
do
    tar xvf $f
    rm $f
done
cp -R $ghc_lib_dir/examples/test-utils ./test-utils-$version_tag
cp -R $ghc_lib_dir/examples/mini-hlint ./mini-hlint-$version_tag
cp -R $ghc_lib_dir/examples/mini-compile ./mini-compile-$version_tag
cat > cabal.project<<EOF
packages:  ghc-lib-parser-$version_tag
         , ghc-lib-$version_tag
         , ghc-lib-parser-ex-$version_tag
         , test-utils-$version_tag
         , mini-hlint-$version_tag
         , mini-compile-$version_tag
EOF

packages=("ghc-lib-parser-$version_tag" "ghc-lib-$version_tag" "ghc-lib-parser-ex-$version_tag" "mini-hlint-$version_tag" "mini-compile-$version_tag")
for p in "${packages[@]}";
do
(cd "$p" && cabal check)
done

rm -rf dist-newstyle

cmd="cabal new-build all -j --ghc-option=-j"
ghc_version=$(ghc -V | tail -c 6)
if [[ "$ghc_version" == "9.2.2" ]]; then
  ffi_inc_path="C_INCLUDE_PATH=$(xcrun --show-sdk-path)/usr/include/ffi"
  eval "$ffi_inc_path" "$cmd"
else
  eval "$cmd"
fi
    
cabal new-run exe:mini-hlint -- mini-hlint-$version_tag/test/MiniHlintTest.hs
cabal new-run exe:mini-compile -- mini-compile-$version_tag/test/MiniCompileTest.hs | tail -10
