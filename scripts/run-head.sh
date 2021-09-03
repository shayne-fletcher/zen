#!/usr/bin/env bash

# Run the ghc-lib build script against the GHC HEAD commit.

if ! [[ -d ./ghc ]]
then
    echo "There is no ghc checkout here to update. Building with 'ghc-flavor' set to 'ghc-master' to get started."
    stack="stack runhaskell --package extra --package optparse-applicative CI.hs -- --ghc-flavor=ghc-master"
    eval $stack
    echo "Run again."
    eval "run-head.sh"
fi

set -euxo pipefail

# Get the latest commit SHA.
HEAD=`cd ghc && \
      git checkout . && \
      git fetch origin && \
      git log origin/master -n 1 | head -n 1 | awk '{ print $2 }'`

# If there's a new release, let's have it.
stack upgrade

# Clean up local references to deleted branches
git remote prune origin

# Build and test ghc-lib against at that commit.
stack runhaskell --package extra --package optparse-applicative CI.hs -- \
      --ghc-flavor $HEAD --no-checkout

# If the above worked out, update CI.hs.
today=`date +'%Y-%m-%d'`
sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs

# Report.
grep "current = .*" CI.hs
