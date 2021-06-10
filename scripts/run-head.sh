#!/usr/bin/env bash

# Run the ghc-lib build script against the GHC HEAD commit.

set -euxo pipefail

# Get the latest commit SHA.
HEAD=`cd ghc && \
      git checkout . && \
      git fetch origin && \
      git log origin/master -n 1 | head -n 1 | awk '{ print $2 }'`

# Build and test ghc-lib against at that commit.
stack runhaskell --package extra --package optparse-applicative CI.hs -- \
      --ghc-flavor $HEAD --no-checkout

# If the above worked out, update CI.hs.
today=`date +'%Y-%m-%d'`
sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs
