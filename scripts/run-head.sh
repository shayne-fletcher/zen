#!/usr/bin/env bash

runhead="run-head.sh"
runhaskell="stack runhaskell --package extra --package optparse-applicative CI.hs"

# If there's a new release, let's have it.
stack upgrade

if ! [[ -d ./ghc ]]
then
    echo "There is no ghc checkout here to update."
    echo "Building with ghc-flavor 'ghc-master' to get started."
    eval "$runhaskell -- --ghc-flavor ghc-master"

    echo "Now restarting to build on the latest GHC commit."
    eval $runhead
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

# Try building these packages with Cabal.
version="0.""`date +'%Y%m%d'`"
(cd ~/tmp&& test-ghc-lib.sh ghc-8.10.7 $version)
