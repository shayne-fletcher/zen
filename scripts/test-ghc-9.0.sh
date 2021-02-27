#!/usr/bin/env bash

set -euxo pipefail

if [ -z "$1" ]
then
    echo "Missing version-tag"
    exit 1
fi

version_tag=$1

echo "cabal-install: $(which cabal)"
echo "cabal-install version: $(cabal -V)"
echo "ghc: $(which ghc)"
echo "ghc version : $(ghc -V)"

ghc_lib_dir=~/project/sf-ghc-lib
ghc_lib_parser_ex_dir=~/project/ghc_lib_parser_ex
build_dir=~/tmp/sf-ghc-lib/ghc-9.0-test-build

mkdir -p $build_dir
cd $build_dir
cp $ghc_lib_dir/ghc-lib-parser-$1.tar.gz .
cp $ghc_lib_dir/ghc-lib-$1.tar.gz .
cp $ghc_lib_parser_ex_dir/ghc-lib-parser-ex$1.tar.gz .
gunzip *.gz
for f in $(ls *.tar)
do
    tar xvf $f
    rm $f
done
cat > cabal.project<<EOF
packages: ghc-lib-parser-$1,ghc-lib-$1,ghc-lib-parser-ex-$1
package ghc-lib-parser-ex
  flags: -auto -no-ghc-lib
EOF
rm -rf dist-newstyle
cabal new-build all
