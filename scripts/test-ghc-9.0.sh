#!/usr/bin/env bash

set -euxo pipefail

if [ -z "$1" ]
then
    echo "Missing ghc-version"
    exit 1
fi
if [ -z "$2" ]
then
    echo "Missing version-tag"
    exit 1
fi

ghc_version=$1
PATH="$HOME/$ghc_version/bin:$PATH"
export PATH

echo "cabal-install: $(which cabal)"
echo "cabal-install version: $(cabal -V)"
echo "ghc: $(which ghc)"
echo "ghc version : $(ghc -V)"

version_tag=$2
ghc_lib_dir=~/project/sf-ghc-lib
ghc_lib_parser_ex_dir=~/project/ghc-lib-parser-ex
build_dir=~/tmp/ghc-lib/$version_tag

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
cp -R $ghc_lib_dir/examples/mini-hlint ./mini-hlint-$version_tag
cp -R $ghc_lib_dir/examples/mini-compile ./mini-compile-$version_tag
cat > cabal.project<<EOF
packages:  ghc-lib-parser-$version_tag
         , ghc-lib-$version_tag
         , ghc-lib-parser-ex-$version_tag
         , mini-hlint-$version_tag
         , mini-compile-$version_tag
package ghc-lib-parser-ex
    flags: -auto -no-ghc-lib
EOF
rm -rf dist-newstyle
cabal new-build all
cabal new-run mini-hlint -- mini-hlint-$version_tag/test/MiniHlintTest.hs
cabal new-run mini-compile -- mini-compile-$version_tag/test/MiniCompileTest.hs | tail -10
