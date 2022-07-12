#!/usr/bin/env bash

# Build and test ghc-lib at either HEAD or the given flavor.
#
# - Full run
#   - `run-head --ghc-flavor=""`, the quickest though is
# - Quickest
#   - `run-head --ghc-flavor="" "--no-checkout --no-builds --no-cabal`

set -exo pipefail

prog=$(basename "$0")
opt_args="
ARG can be a flavor or the empty string e.g. --ghc-flavor=\"\"
OPTS is a quoted string with contents e.g: \"--no-checkout --no-builds --no-cabal\""
usage="usage: $prog --ghc-flavor=ARG OPTS""
$opt_args"

GHC_FLAVOR=""
# ARG
if [ ! -z "$1" ]
then
    if [[ "$1" == "--help" ]]; then
        echo "$usage" && exit 0
    elif [[ "$1" =~ --ghc-flavor=(.*)$ ]]; then
      GHC_FLAVOR="${BASH_REMATCH[1]}"
    else
        # there is some "first" argument but it's not a ghc-flavor as
        # far as we can tell
        echo "ghc-flavor expected in first argument"
        echo "$usage" && exit 0
    fi
fi

# OPTS
opts=""
if [ ! -z "$2" ]; then
    opts="$2"
fi

set -u

no_checkout=""
if [[ "$opts" == *"--no-checkout"* ]]; then
  no_checkout="--no-checkout"
  echo "cloning ghc skipped."
fi
no_builds=""
if [[ "$opts" == *"--no-builds"* ]]; then
  no_builds="--no-builds"
  echo "ghc-lib package & examples building skipped."
fi
no_cabal=""
if [[ "$opts" == *"--no-cabal"* ]]; then
  no_cabal="--no-cabal"
  echo "hlint stack as a cabal.project building skipped."
fi

packages="--package extra --package optparse-applicative"
runhaskell="stack runhaskell $packages"
DOLLAR="$"
locals="locals"
everything="everything"

if ! [[ -f ./ghc-lib-gen.cabal ]]; then
    echo "Missing 'ghc-lib-gen.cabal'."
    echo "This script should be executed from a ghc-lib checkout directory."
    exit 1
fi

if ! [[ -d ./ghc ]]; then
    echo "There is no ghc checkout here to update."
    echo "Building with ghc-flavor 'ghc-master' to get started."
n    eval "$runhaskell CI.hs -- --ghc-flavor ghc-master"

    echo "Now restarting build at the latest GHC commit."
fi

# If there's a new release, let's have it.
stack upgrade

# It's common for the git fetch step to report errors of the form
# "fatal: remote error: upload-pack: not our ref SHA culminating with
# "Errors during submodule fetch:..." and exit with a non-zero code.
# The --recurse-submodules=no is an attempt to prevent this.
(cd ghc && git checkout . && \
     git fetch origin --prune --tags --recurse-submodules=no \
)
if [ -z "$GHC_FLAVOR" ]; then
  # Get the latest commit SHA.
  HEAD=$(cd ghc && git log origin/master -n 1 | head -n 1 | awk '{ print $2 }')
  if [ -z "$HEAD" ]; then
      echo "\$HEAD is empty. Trying over." && run-head
  fi
  # If $HEAD agrees with the "last tested at" SHA in CI.hs stop here.
  current=$(grep "current = .*" CI.hs | grep -o "\".*\"" | cut -d "\"" -f 2)
  if [[ "$current" == "$HEAD" ]]; then
    echo "The last \"tested at\" SHA (\"$current\") hasn't changed"
    exit 0
  fi
fi

today=$(date -u +'%Y-%m-%d')
if [[ -z "$GHC_FLAVOR" \
   || "$GHC_FLAVOR" == "ghc-master" ]]; then
  version="0.""`date -u +'%Y%m%d'`"
else
  flavor=$([[ "$GHC_FLAVOR" =~ (ghc\-)([0-9])\.([0-9])\.([0-9]) ]] && echo "${BASH_REMATCH[2]}.${BASH_REMATCH[3]}.${BASH_REMATCH[4]}")
  version="$flavor"".""$(date -u +'%Y%m%d')"
fi

# ghc-lib

cmd="$runhaskell CI.hs -- $no_checkout $no_builds --ghc-flavor "
[ -z "$GHC_FLAVOR" ] && eval "$cmd" "$HEAD" || eval "$cmd" "$GHC_FLAVOR"
sha_ghc_lib_parser=`shasum -a 256 $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz | awk '{ print $1 }'`

if [ -z "$GHC_FLAVOR" ]; then
    # If the above worked out, update CI.hs.
    sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs
    # Report.
    grep "current = .*" CI.hs
fi

# ghc-lib-parser-ex

cd ../ghc-lib-parser-ex && git checkout .
branch=$(git rev-parse --abbrev-ref HEAD)

if [[ -z "$GHC_FLAVOR" \
   || "$GHC_FLAVOR" == "ghc-master" ]]; then
  if [[ "$branch" != "ghc-next" ]]; then
    echo "Not on ghc-next. Trying 'git checkout ghc-next'"
    git checkout ghc-next
  fi
  if [[ "$branch" != "ghc-next" ]]; then
    echo "Not on ghc-next. Trying 'git checkout ghc-next'"
    git checkout ghc-next
  fi
elif [[ "$branch" != "master" ]]; then
  echo "Not on master. Trying 'git checkout master'"
  git checkout master
fi

cat > stack-head.yaml <<EOF
resolver: nightly-2022-05-27 # ghc-9.2.2
extra-deps:
  - archive: $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha_ghc_lib_parser"
ghc-options:
    "$DOLLAR$everything": -j
    "$DOLLAR$locals": -ddump-to-file -ddump-hi -Wall -Wno-name-shadowing -Wunused-imports
flags:
  ghc-lib-parser-ex:
    auto: false
    no-ghc-lib: false
packages:
  - .
EOF

stack_yaml="--stack-yaml stack-head.yaml"
eval "$runhaskell $stack_yaml CI.hs -- $no_builds $stack_yaml --version-tag $version"
sha_ghc_lib_parser_ex=`shasum -a 256 $HOME/project/ghc-lib-parser-ex/ghc-lib-parser-ex-$version.tar.gz | awk '{ print $1 }'`

# Hlint

cd ../hlint && git checkout .
branch=$(git rev-parse --abbrev-ref HEAD)
if [[ -z "$GHC_FLAVOR" \
   || "$GHC_FLAVOR" == "ghc-master" ]]; then
  if [[ "$branch" != "ghc-next" ]]; then
    echo "Not on ghc-next. Trying 'git checkout ghc-next'"
    git checkout ghc-next
  fi
elif [[ "$GHC_FLAVOR" == "ghc-9.2.2" \
   || "$GHC_FLAVOR" == "ghc-9.2.3" ]]; then
  if [[ "$branch" != "master" ]]; then
    echo "Not on master. Trying 'git checkout master'"
    git checkout master
  fi
elif [[ "$GHC_FLAVOR" == "ghc-9.4.1" ]]; then
  if [[ "$branch" != "ghc-9.4" ]]; then
    echo "Not on ghc-9.4. Trying 'git checkout ghc-9.4'"
    git checkout ghc-9.4
  fi
fi

cat > stack-head.yaml <<EOF
resolver: nightly-2022-06-10 # ghc-9.2.3
packages:
  - .
extra-deps:
  - archive: $HOME/project/sf-ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha_ghc_lib_parser"
  - archive: $HOME/project/ghc-lib-parser-ex/ghc-lib-parser-ex-$version.tar.gz
    sha256: "$sha_ghc_lib_parser_ex"
ghc-options:
    "$DOLLAR$everything": -j
    "$DOLLAR$locals": -ddump-to-file -ddump-hi -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans
flags:
 hlint:
   ghc-lib: true
 ghc-lib-parser-ex:
   auto: false
   no-ghc-lib: false
# Allow out-of-bounds ghc-lib-parser and ghc-lib-parser-ex.
allow-newer: true
EOF

eval "stack $stack_yaml build"
eval "stack $stack_yaml run -- --test"

# --
# - phase: test-ghc-9.0.sh
#   (test building the hlint stack as a cabal.project)

if [ "$no_cabal" == --no-cabal ]; then
  echo "hlint as a cabal.project skipped (and now my watch is ended)."
  exit 0
else
  echo "--
  hlint as a cabal.project.
"
fi

# It's so annoying. I just cannot get 'allow-newer' to work in this
# context. Well, never mind; take the approach of constraining the
# bounds exactly. It's kind of more explicitly saying what we mean
# anyway.
sed -i '' "s/^version:.*\$/version:            $version/g" hlint.cabal
sed -i '' "s/^.*ghc-lib-parser ==.*\$/          ghc-lib-parser == $version/g" hlint.cabal
sed -i '' "s/^.*ghc-lib-parser-ex >=.*\$/          ghc-lib-parser-ex == $version/g" hlint.cabal
eval "stack $stack_yaml sdist . --tar-dir ."

# `cabal new-build all`

(cd ~/tmp && test-ghc-9.0.sh                                   \
     --ghc-version=ghc-9.2.3                                   \
     --version-tag="$version"                                  \
     --ghc-lib-dir="$HOME/project/sf-ghc-lib"                  \
     --ghc-lib-parser-ex-dir="$HOME/project/ghc-lib-parser-ex" \
     --hlint-dir="$HOME/project/hlint"                         \
     --build-dir="$HOME/tmp/ghc-lib/$version"                  \
)
