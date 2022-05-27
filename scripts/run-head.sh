#!/usr/bin/env bash

runhaskell="stack runhaskell --package extra --package optparse-applicative CI.hs"
DOLLAR="$"
locals="locals"

if ! [[ -f ./ghc-lib-gen.cabal ]]
then
    echo "Missing 'ghc-lib-gen.cabal'."
    echo "This script should be executed from a ghc-lib checkout directory."
    exit 1
fi

if ! [[ -d ./ghc ]]
then
    echo "There is no ghc checkout here to update."
    echo "Building with ghc-flavor 'ghc-master' to get started."
    eval "$runhaskell -- --ghc-flavor ghc-master"

    echo "Now restarting build at the latest GHC commit."
fi

# If there's a new release, let's have it.
stack upgrade

# Run the ghc-lib build script against the GHC HEAD commit.

set -euxo pipefail

# It's common for the git fetch step to report errors of the form
# "fatal: remote error: upload-pack: not our ref SHA culminating with
# "Errors during submodule fetch:..." and exit with a non-zero code.
# The --recurse-submodules=no is an attempt to prevent this.
(cd ghc && git checkout . && git fetch origin --recurse-submodules=no && git remote prune origin)
# Get the latest commit SHA.
HEAD=$(cd ghc && git log origin/master -n 1 | head -n 1 | awk '{ print $2 }')
if test -z "$HEAD"
then
    echo "\$HEAD is empty. Trying over."
    run-head
fi

today=`date -u +'%Y-%m-%d'`
version="0.""`date -u +'%Y%m%d'`"

# Build and test ghc-lib against at that commit.
eval "$runhaskell -- --ghc-flavor $HEAD --no-checkout"
sha_ghc_lib_parser=`shasum -a 256 $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz | awk '{ print $1 }'`

# If the above worked out, update CI.hs.
sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs
# Report.
grep "current = .*" CI.hs

# Build and test ghc-lib-parser-ex with this ghc-lib-parser.

cd ../ghc-lib-parser-ex
branch=`git rev-parse --abbrev-ref HEAD`
if [[ "$branch" != "ghc-next" ]]; then
  echo "Not on ghc-next. Trying 'git checkout ghc-next'"
  git checkout ghc-next
fi
# We assume ghc-next is a copy of origin managed with a strategy of
# periodic rebase on master and force-push. Don't try to fetch and
# merge.

cat > stack-head.yaml <<EOF
resolver: nightly-2022-05-27 # ghc-9.2.2
extra-deps:
  - archive: $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha_ghc_lib_parser"
ghc-options:
    "$DOLLAR$locals": -ddump-to-file -ddump-hi -Wall -Wno-name-shadowing -Wunused-imports
flags:
  ghc-lib-parser-ex:
    auto: false
    no-ghc-lib: false
packages:
  - .
EOF

eval "$runhaskell -- --stack-yaml stack-head.yaml --version-tag $version"
sha_ghc_lib_parser_ex=`shasum -a 256 $HOME/project/ghc-lib-parser-ex/ghc-lib-parser-ex-$version.tar.gz | awk '{ print $1 }'`

# Try 'cabal newbuild all' w/ghc-9.2.2.
(cd ~/tmp&& test-ghc-9.0.sh ghc-9.2.2 $version)

# Hlint

cd ../hlint
branch=`git rev-parse --abbrev-ref HEAD`
if [[ "$branch" != "ghc-next" ]]; then
  echo "Not on ghc-next. Trying 'git checkout ghc-next'"
  git checkout ghc-next
fi
# We assume ghc-next is a copy of origin managed with a strategy of
# periodic rebase on master and force-push. Don't try to fetch and
# merge. We might one day perhaps automate rebasing on master here?

cat > stack-head.yaml <<EOF
resolver: nightly-2022-05-27 # ghc-9.2.2
packages:
  - .
extra-deps:
  - archive: $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha_ghc_lib_parser"
  - archive: $HOME/project/ghc-lib-parser-ex/ghc-lib-parser-ex-$version.tar.gz
    sha256: "$sha_ghc_lib_parser_ex"
ghc-options: {"$DOLLAR$locals": -ddump-to-file -ddump-hi -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans}
flags:
 hlint:
   ghc-lib: true
 ghc-lib-parser-ex:
   auto: false
   no-ghc-lib: false
# Allow out-of-bounds ghc-lib-parser and ghc-lib-parser-ex.
allow-newer: true
EOF

# Build & test hlint
eval "stack --stack-yaml stack-head.yaml build"
eval "stack --stack-yaml stack-head.yaml run -- --test"
