#!/usr/bin/env bash

set -exo pipefail

prog=$(basename "$0")
opt_args="
ARG is a directory for git repo clones e.g. --repo-dir=$HOME/project
OPTS is a quoted string with contents e.g: \"\""
usage="usage: $prog ARG OPTS""
$opt_args"

repo_dir=""
# ARG
if [ -n "$1" ]
then
    if [[ "$1" == "--help" ]]; then
        echo "$usage" && exit 0
    elif [[ "$1" =~ --repo-dir=(.*)$ ]]; then
      repo_dir="${BASH_REMATCH[1]}"
    else
        # There is some "first" argument but it's not a repo-dir as
        # far as we can tell.
        printf "repo-dir expected in first argument\n%s" "$usage\n" && exit 0
    fi
fi

set -u

if [[ ! -d "$repo_dir" ]]; then
    mkdir -p "$repo_dir"
fi

pushd "$repo_dir"

if [[ ! -d "ghc-lib" ]]; then
    git clone git@github.com:shayne-fletcher/ghc-lib.git
    pushd "ghc-lib"
    git fetch origin --tags
    git clone --recursive git@gitlab.haskell.org:ghc/ghc.git
    pushd "ghc"
    git fetch origin --tags
    popd
    popd
fi
if [[ ! -d "ghc-lib-parser-ex" ]]; then
    git clone git@github.com:shayne-fletcher/ghc-lib-parser-ex.git
    pushd "ghc-lib-parser-ex"
    git fetch origin --tags
    popd
fi
if [[ ! -d "hlint" ]]; then
    git clone git@github.com:ndmitchell/hlint.git
    pushd "hlint"
    git fetch origin --tags
    popd
fi
