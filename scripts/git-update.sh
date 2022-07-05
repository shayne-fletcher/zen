#!/usr/bin/env bash

set -euxo pipefail

project="$HOME/project"

repos=("$project/zen/" \
       "$project/sf-ghc-lib" \
       "$project/ghc-lib-parser-ex" \
       "$project/hlint" \
)
for repo in "${repos[@]}"; do
    (cd "$repo" && \
         git checkout . && \
         git checkout master && \
         git fetch --tags --prune origin && \
         git merge origin/master \
    )
done

repos=("$project/ghc-lib-parser-ex" "$project/hlint")
for repo in "${repos[@]}"; do
  set +e
  (cd "$repo && git branch -D ghc-next")
  set -e

  (cd "$repo" && eval "git checkout -t origin/ghc-next")
done
