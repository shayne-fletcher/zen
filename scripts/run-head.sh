#!/usr/bin/env bash

runhaskell="stack runhaskell --package extra --package optparse-applicative CI.hs"

# If there's a new release, let's have it.
stack upgrade

if ! [[ -d ./ghc ]]
then
    echo "There is no ghc checkout here to update."
    echo "Building with ghc-flavor 'ghc-master' to get started."
    eval "$runhaskell -- --ghc-flavor ghc-master"

    echo "Now restarting build at the latest GHC commit."
fi

# Run the ghc-lib build script against the GHC HEAD commit.

set -euxo pipefail

# Get the latest commit SHA.
HEAD=`cd ghc && \
      git checkout . && \
      git fetch origin && \
      git log origin/master -n 1 | head -n 1 | awk '{ print $2 }'`

# Clean up local references to deleted branches
git remote prune origin

# Build and test ghc-lib against at that commit.
eval "$runhaskell -- --ghc-flavor $HEAD --no-checkout"

# If the above worked out, update CI.hs.
today=`date +'%Y-%m-%d'`
sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs

# Report.
grep "current = .*" CI.hs

version="0.""`date +'%Y%m%d'`"

# Build and test ghc-lib-parser-ex with this ghc-lib-parser.

cd ../ghc-lib-parser-ex
branch=`git rev-parse --abbrev-ref HEAD`
if [[ "$branch" != "master" ]]; then
  echo "Not on master. Trying 'git checkout master'"
  git checkout master
fi
git fetch origin
git checkout .
git merge origin/master

DOLLAR="$"
locals="locals"
sha=`shasum -a 256 $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz | awk '{ print $1 }'`
echo $sha
cat > stack-head.yaml <<EOF
resolver: nightly-2021-03-31 # ghc-8.10.4
extra-deps:
  - archive: $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha"
ghc-options:
    "$DOLLAR$locals": -ddump-to-file -ddump-hi -Wall -Wno-name-shadowing -Wunused-imports
flags:
  ghc-lib-parser-ex:
    auto: false
    no-ghc-lib: false
packages:
  - .
EOF
eval "$runhaskell -- --stack-yaml stack-head.yaml"

# Try 'cabal newbuild all' w/ghc-9.0.1.
(cd ~/tmp&& test-ghc-9.0.sh ghc-9.0.1 $version)
