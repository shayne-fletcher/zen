# Record Dot Syntax implementation notes

- Branch `wip/T18599`
- [T18599](https://gitlab.haskell.org/ghc/ghc/-/issues/18599)
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
