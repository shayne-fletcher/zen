#!/usr/bin/env bash

# Build and test ghc-lib at HEAD or the given flavor.
#
# - Never run before?
#  - `run-head --init=/path/to/repo-dir`
#    - this will clone ghc, stack, ghc-lib, ghc-lib-parser-ex and
#      hlint into the given path (no use is made of ghc at this time)
#    - thereafter pass (in `opts`) `--repo-dir=/path/to/repo-dir` to
#      `run-head` (default if omitted:`$HOME/project`)

# - Full
#   - `run-head --ghc-flavor=""`, the quickest though is
# - Quickest
#   - `run-head --ghc-flavor="" "--no-checkout --no-builds --no-cabal --no-haddock`

set -exo pipefail

prog=$(basename "$0")
opt_args="
ARG can be a flavor or the empty string e.g. --ghc-flavor=\"\"
OPTS is a quoted string with contents e.g: \"--no-checkout --no-builds --no-cabal --no-haddock\""
usage="usage: $prog --ghc-flavor=ARG OPTS""
$opt_args"

GHC_FLAVOR=""
# ARG
if [ -n "$1" ]
then
    if [[ "$1" == "--help" ]]; then
        echo "$usage" && exit 0
    elif [[ "$1" =~ --ghc-flavor=(.*)$ ]]; then
      GHC_FLAVOR="${BASH_REMATCH[1]}"
    elif [[ "$1" =~ --init=(.*)$ ]]; then
      init_arg="${BASH_REMATCH[1]}"
      run-head-init --repo-dir="$init_arg"
      echo "repo-dir \"$repo_dir\" initialized"
      echo "next: run-head --ghc-flavor=... ... --repo-dir=$repo_dir"
      exit 0
    else
        echo "expected --ghc-flavor=... or --init=... in first argument"
        echo "$usage" && exit 0
    fi
fi

# OPTS
opts=""
if [ -n "$2" ]; then
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
stack_yaml=""
stack_yaml_flag=""
if [[ "$opts" =~ (.*)--stack-yaml=([^[:space:]]+) ]]; then
  stack_yaml="${BASH_REMATCH[2]}"
  stack_yaml_flag="--stack-yaml $stack_yaml"
fi
resolver=""
resolver_flag=""
if [[ "$opts" =~ (.*)--resolver=([^[:space:]]+) ]]; then
  resolver="${BASH_REMATCH[2]}"
  resolver_flag="--resolver $resolver"
fi
repo_dir="$HOME/project"
if [[ "$opts" =~ (.*)--repo-dir=([^[:space:]]+) ]]; then
  repo_dir="${BASH_REMATCH[2]}"
fi
with_haddock_flag="--with-hadock"
if [[ "$opts" == *"--no-haddock"* ]]; then
  with_haddock_flag=""
  echo "generation haddocks skipped."
fi

echo "stack-yaml: $stack_yaml"
echo "stack-yaml flag: $stack_yaml_flag"
echo "resolver: $resolver"
echo "resolver flag: $resolver_flag"
echo "repo-dir: $repo_dir"

packages="--package extra --package optparse-applicative"
runhaskell="stack runhaskell $packages"
DOLLAR="$"
locals="locals"
everything="everything"

# If there's a new release, let's have it.
if [ 1 ]; then
  cd "$repo_dir/stack"
  git fetch origin && git merge origin/master
  stack --stack-yaml=stack-macos.yaml install
else
  stack upgrade # Upgrade to the latest official
fi

cd "$repo_dir"/ghc-lib

if ! [[ -f ./ghc-lib-gen.cabal ]]; then
    echo "Missing 'ghc-lib-gen.cabal'."
    echo "This script should be executed from a ghc-lib checkout directory."
    exit 1
fi

if ! [[ -d ./ghc ]]; then
    echo "There is no ghc checkout here to update."
    echo "Building with ghc-flavor 'ghc-master' to get started."
    eval "$runhaskell $stack_yaml_flag $resolver_flag CI.hs -- $stack_yaml_flag $resolver_flag --ghc-flavor ghc-master"
    echo "Now restarting build at the latest GHC commit."
fi

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
  version="0.""$(date -u +'%Y%m%d')"
else
  flavor=$([[ "$GHC_FLAVOR" =~ (ghc\-)([0-9])\.([0-9])\.([0-9]) ]] && echo "${BASH_REMATCH[2]}.${BASH_REMATCH[3]}.${BASH_REMATCH[4]}")
  version="$flavor"".""$(date -u +'%Y%m%d')"
fi

# ghc-lib

cmd="$runhaskell $stack_yaml_flag $resolver_flag CI.hs -- $stack_yaml_flag $resolver_flag $no_checkout $no_builds --ghc-flavor "
if [ -z "$GHC_FLAVOR" ]; then
    eval "$cmd" "$HEAD"
else
    eval "$cmd" "$GHC_FLAVOR"
fi
sha_ghc_lib_parser=$(shasum -a 256 "$repo_dir"/ghc-lib/ghc-lib-parser-"$version".tar.gz | awk '{ print $1 }')

if [ -z "$GHC_FLAVOR" ]; then
    # If the above worked out, update CI.hs.
    sed -i '' "s/current = \".*\" -- .*/current = \"$HEAD\" -- $today/g" CI.hs
    # Report.
    grep "current = .*" CI.hs
fi

# ghc-lib-parser-ex

cd ../ghc-lib-parser-ex && git checkout .
branch=$(git rev-parse --abbrev-ref HEAD)

# if the flavor indicates ghc's master branch get on
# ghc-lib-parser-ex's 'ghc-next' branch ...
if [[ -z "$GHC_FLAVOR" \
   || "$GHC_FLAVOR" == "ghc-master" ]]; then
  if [[ "$branch" != "ghc-next" ]]; then
    echo "Not on ghc-next. Trying 'git checkout ghc-next'"
    git checkout ghc-next
  fi
#... else it's a released flavor, get on branch ghc-lib-parser-ex's
#'master' branch
else
  if [[ "$branch" != "master" ]]; then
      echo "Not on master. Trying 'git checkout master'"
      git checkout master
  fi
fi

# If a resolver hasn't been set, set it now to this.
[[ -z "$resolver" ]] && resolver=nightly-2022-08-04 # ghc-9.2.4

# This an elaborate step to create a config file'stack-head.yaml'.
#
# If a stack-yaml argument was provided, seed its contents from it
# otherwise, assume a curated $resolver and create it from scratch.
if [[ -n "$stack_yaml" ]]; then
  echo "Seeding stack-head.yaml from $stack_yaml"
  # shellcheck disable=SC2002
  cat "$stack_yaml" | \
  # Delete any pre-existing ghc-lib-parser extra dependency.
  sed -e "s;^.*ghc-lib-parser.*$;;g" | \
  sed -e "s;^extra-deps:$;\
extra-deps:\n\
  # ghc-lib-parser\n\
  - archive: ${repo_dir}/ghc-lib/ghc-lib-parser-${version}.tar.gz\n\
    sha256: \"${sha_ghc_lib_parser}\";\
g" | \
  sed -e "s;^resolver:.*$;resolver: ${resolver};g" > stack-head.yaml
else
  cat > stack-head.yaml <<EOF
resolver: $resolver
extra-deps:
  - archive: ${repo_dir}/ghc-lib/ghc-lib-parser-$version.tar.gz
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
fi

stack_yaml=stack-head.yaml
stack_yaml_flag="--stack-yaml $stack_yaml"
# No need to pass $resolver_flag here, we fixed the resolver in
# 'stack-head.yaml'.
eval "$runhaskell $stack_yaml_flag CI.hs -- $no_builds $stack_yaml_flag --version-tag $version"
sha_ghc_lib_parser_ex=$(shasum -a 256 "$repo_dir"/ghc-lib-parser-ex/ghc-lib-parser-ex-"$version".tar.gz | awk '{ print $1 }')

# Hlint

cd ../hlint && git checkout .
branch=$(git rev-parse --abbrev-ref HEAD)
# if the flavor indicates ghc's master branch get on hlint's
# 'ghc-next' branch ...
if [[ -z "$GHC_FLAVOR" \
   || "$GHC_FLAVOR" == "ghc-master" ]]; then
  if [[ "$branch" != "ghc-next" ]]; then
    echo "Not on ghc-next. Trying 'git checkout ghc-next'"
    git checkout ghc-next
  fi
#... else it's a released flavor, get on branch hlint's 'master'
#branch
else
  if [[ "$branch" != "master" ]]; then
      echo "Not on master. Trying 'git checkout master'"
      git checkout master
  fi
fi

# We're stuck with only curated resolvers for hlint at this time.
resolver=nightly-2022-08-04 # ghc-9.2.4

cat > stack-head.yaml <<EOF
resolver: $resolver
packages:
  - .
extra-deps:
  - archive: $repo_dir/ghc-lib/ghc-lib-parser-$version.tar.gz
    sha256: "$sha_ghc_lib_parser"
  - archive: $repo_dir/ghc-lib-parser-ex/ghc-lib-parser-ex-$version.tar.gz
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

# Again, it would be wrong to pass $resolver_flag here.
eval "stack" "$stack_yaml_flag" "build"
eval "stack" "$stack_yaml_flag" "run" "--" "--test"

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
eval "stack" "$stack_yaml_flag" "sdist" "." "--tar-dir" "."

# - Generate a cabal.project of
#   - ghc-lib, ghc-lib-parser-ex, examples, hlint
#     - (somwhere like ~/tmp/ghc-lib/ghc-lib-9.4.2.20220821/ghc-9.4.1/cabal.project
# - and `cabal new-build all`.
# - Maybe produce haddocks too
#   - Depending on the contents of `$with_haddock_flag`. Also,
# - Run ghc-lib-test-mini-hlint, ghc-lib-test-mini-compile and the
#   hlint test suite.
tmp_dir="$HOME/tmp"
mkdir -p "$tmp_dir"
(cd "$HOME"/tmp && run-head-cabal-build-test.sh            \
     --ghc-version=ghc-9.4.2                               \
     --version-tag="$version"                              \
     --ghc-lib-dir="$repo_dir/ghc-lib"                     \
     --ghc-lib-parser-ex-dir="$repo_dir/ghc-lib-parser-ex" \
     --hlint-dir="$repo_dir/hlint"                         \
     --build-dir="$tmp_dir/ghc-lib/$version"               \
     "$with_haddock_flag"                                  \
)
