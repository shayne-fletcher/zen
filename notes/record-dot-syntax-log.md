# Record Dot Syntax implementation notes

# Tasks and diffs

- Branch `wip/T18599`
- [ghc branch wip/T18599](https://gitlab.haskell.org/ghc/ghc/-/issues/18599)
- [haddock branch wip/T18599](https://gitlab.haskell.org/ghc/haddock/-/commits/wip/T18599)
- [MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4532)
- [Parsing Record Dot Syntax](https://github.com/shayne-fletcher/zen/blob/master/notes/field-updates.html)
- [Parsing Record Dot Syntax (Rendered)](file:///Users/shayne/project/zen/notes/field-updates.html)
- Build:

```bash
rm -rf _build&&git submodule update --recursive
./boot&&./configure`
./hadrian/build-stack --flavour=quickest -j
```
- Test

```
#!/usr/bin/env bash

set -euo pipefail

PATH=`pwd`/_build/stage1/bin:$PATH; export PATH

tests=(                    \
      RecordDotSyntax      \
      RecordDotSyntaxFail0 \
      RecordDotSyntaxFail1 \
      RecordDotSyntaxFail2 \
      RecordDotSyntaxFail3 \
      RecordDotSyntaxFail4 \
 )

test () {
  printf "make test TEST=$%s\n" $1
  make test TEST=$1
}

for t in "${tests[@]}"
do
  test $t
done
```

## Rebasing

- Do your rebasing on the `wip/T18599-rebase` branch
  - When you're satisfied with it you can `git push -f origin wip/T18599-rebase:wip/T18599`
### Prepare
- Its a good idea to update master and build before embarking on rebasing
- This wil uncover any problems with the build before you get into rebasing
  - For example, today `hadrian/build-stack --flavour=quickest` appears to have issues linking the stage 1 bin/ghc;
  - I switched to `make -j` in this case.
### Dealing with Haddock
- I found the best thing to do is:
  - Work out the commit SHA that ghc `origin/master` is on (`X` say)
  - Rebuild the Record Dot Syntax branch off that commit:

```
cd ~/project/haddock
git checkout master&& git push origin :wip/T18599
git branch -D wip/T18599
git checkout X && git checkout -b wip/T18599
```
  - Add the single line change:
    - `/haddock/haddock-api/src/Haddock/Backends/Hyperlinker/Parser.hs`
      - Add `ITproj -> TkOperator` after the line for `ITDot`
  - Push the newly built branch `git push origin wip/T18599:wip/T18599`
  - Record the commit SHA `XX` (say)

#### Rebase ghc
- I tend to do this on a branch off `wip/T18599`
  - `git checkout wip/T18599&&git checkout -b wip/T18599-rebase`
- Start the rebase `git fetch origin&&git rebase origin/master`:
  - Fix haddock:
    - Update haddock `cd utils/haddock&& git fetch origin&& git checkout XXX`
    - Resolve the merge conflict `git add utils/haddock`
- Useful command:
  - Have their version: `git checkout --theirs path/to/foo&&git add path/to/foo`
- When you are done, `git reset --soft H` where `H` is the new `HEAD`, `git add .&&git commit -m "Record dot syntax"`
- Check everything builds & tests
- Overwrite the branch with the now rebased one `git push -f origin wip/T18599-base:wip/T18599`
