# Record Dot Syntax implementation notes

# Tasks and diffs

- Branch `wip/T18599`
- [ghc branch wip/T18599](https://gitlab.haskell.org/ghc/ghc/-/issues/18599)
- [haddock branch wip/T18599](https://gitlab.haskell.org/ghc/haddock/-/commits/wip/T18599)
- [MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4532)
- [Parsing Record Dot Syntax](https://github.com/shayne-fletcher/zen/blob/master/notes/field-updates.html)
- [Parsing Record Dot Syntax (Rendered)](file:///Users/shayne/project/zen/notes/field-updates.html)
- [Improve Handling of Overloaded Labels...](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4981) (Simon's branch)

  - [work on branch wip/T19154](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4981#:~:text=merge-,wip/T19154,-into)
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
        RecordDotSyntax1     \
        RecordDotSyntax2     \
        RecordDotSyntax3     \
        RecordDotSyntaxFail0 \
        RecordDotSyntaxFail1 \
        RecordDotSyntaxFail2 \
        RecordDotSyntaxFail3 \
        RecordDotSyntaxFail4 \
        RecordDotSyntaxFail5 \
        RecordDotSyntaxFail6 \
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
    - Here's one way:
      - `cd utils && git ls-tree master | grep haddock | awk '{ print $3 }'`
  - Rebuild the Record Dot Syntax branch off that commit:

    ```
    cd ~/project/haddock
    git checkout master&& git push origin :wip/T18599
    git branch -D wip/T18599
    git checkout X && git checkout -b wip/T18599
    ```
  - Add the single line change:
    - `/haddock/haddock-api/src/Haddock/Backends/Hyperlinker/Parser.hs`
      - Add `ITproj -> TkOperator` after the line for `IdDot`
  - Push the newly built branch `git push origin wip/T18599:wip/T18599`
  - Record the commit SHA `XX` (say)

#### Rebase ghc
- I tend to do this on a branch off `wip/T18599`
  - `git checkout wip/T18599&&git checkout -b wip/T18599-rebase`
- Start the rebase `git fetch origin&&git rebase origin/master`:
  - Fix haddock:
    - Update haddock `cd utils/haddock&& git fetch origin&& git checkout XXX`
    - Resolve the merge conflict `git add utils/haddock`
- Useful commands:
  - Discard local changes: `git checkout --ours path/to/foo&&git add path/to/foo`
  - Discard other's changes: `git checkout --theirs path/to/foo&&git add path/to/foo`
- When you are done, `git reset --soft H` where `H` is the new `HEAD`, `git add .&&git commit -m "Record dot syntax"`
- Check everything builds & tests
- Overwrite the branch with the now rebased one `git push -f origin wip/T18599-base:wip/T18599`

---

## Rebasing on wip/T19154 (Simon's branch)

- Branched from:
  ```
  commit 40983d2331fe34c0af6925db7588d5ac6a19ae36 (origin/master, origin/HEAD)
  Sun Feb 7
  ```
- It appears that on that branch, haddock is at this revision:
  ```
    010f0320dff64e3f86091ba4691bc69ce6999647
  ```
- So fixing haddock:
  ```
    cd ~/project/haddock
    git checkout master
    git push origin :wip/T18599
    git branch -D wip/T18599
    git checkout 010f0320dff64e3f86091ba4691bc69ce6999647
    git checkout -b wip/T18599
sed -i '' 's/    ITdot                  -> TkOperator/    ITdot                  -> TkOperator\
    ITproj              {} -> TkOperator/g' \
    haddock-api/src/Haddock/Backends/Hyperlinker/Parser.hs
    git add .
    git commit -m "Add ITproj to parser"
    git push origin wip/T18599:wip/T18599
    git log | awk 'NR==1{print $2}' # Get the commit SHA
  ```
- The resulting commit SHA is `847eab3ab471c6097eadec7bd7818e0b4b79fb87`.
- Onto the rebase:
  ```
    cd ~/project/ghc
    git checkout wip/T18599
    git checkout -b wip/T18599-rebase
    git fetch origin&&git rebase origin/wip/T19154
    (cd utils/haddock&&git fetch origin&&git checkout 847eab3ab471c6097eadec7bd7818e0b4b79fb87) && git add utils/haddock
    # ... Deal with the remaining rebase conflicts (wasn't easy but then, wasn't hard either)
    git rebase --continue
  ```
--

- To toggle draft status of an MR, enter `/wip` in a comment (no other text)
  - It's a toggle; put the MR back into draft by `/wip` again.

---
## Record dot syntax landing 2021-03-06

- [!4532](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4532)
- Commit SHA: `06f1170bed5237766b53306a9ad088e4b151939e`
- With `wip/T18599`:

  - Depends on:

    - [haddock `wip/T18599`](https://gitlab.haskell.org/ghc/haddock/-/tree/wip/T18599)
    - Commit SHA: `0bf811ba98af90f852066734977aacb898ba8e69`
- [Pipeline](https://gitlab.haskell.org/ghc/ghc/-/pipelines/32554)

--

# Record update syntax

Task : [Refactor `RecordUpd` task](https://gitlab.haskell.org/ghc/ghc/-/issues/19463).

- At the time `RecordDotSyntax` landed, `RecordUpd` was defined like this:

  ```
  | RecordUpd
      { rupd_ext  :: XRecordUpd p
      , rupd_expr :: LHsExpr p
      , rupd_flds :: Either [LHsRecUpdField p] [LHsRecUpdProj p]
      }
  ```

- Where:

  - `HsRecField' id arg`:

    ```
    data HsRecField' id arg = HsRecField {
        hsRecFieldLbl :: Located id
      , hsRecFieldArg :: arg
      , hsRecPun :: Bool
    }
    ```
  - `LHsRecUpdField p`:

    ```
    type LHsRecField' p arg = Located (HsRecField' p arg)
    type LHsRecField  p arg = Located (HsRecField  p arg)
    type LHsRecUpdField p = Located (HsRecUpdField p)
    type HsRecField p arg = HsRecField' (FieldOcc p) arg
    type HsRecUpdField p  = HsRecField' (AmbiguousFieldOcc p) (LHsExpr p)
    ```
  - `LHsRecUpdProj p`:

    ```
    newtype FieldLabelStrings = FieldLabelStrings [Located FieldLabelString]
    type RecProj arg = HsRecField' FieldLabelStrings arg
    type LHsRecProj p arg = Located (RecProj arg)
    type RecUpdProj p = RecProj (LHsExpr p)
    type LHsRecUpdProj p = Located (RecUpdProj p)
    ```

- The critical change to make is the representation of `HsRecUpdField` to one based on a `NonEmpty` list of `AmbiguousFieldOcc`:

  ```
    type HsRecUpdField p = HsRecField' (NonEmpty (AmbiguousFieldOcc p)) (LHsExpr p)
  ```

- Then all field updates are encoded as `HsRecUpdField`s:
  - Normal updates are singleton `AmbigousFieldOcc p` lists
  - Record dot updates are encoded into non-singleton `AmbiguousFieldOcc p` lists

  ```
  | RecordUpd
      { rupd_ext  :: XRecordUpd p
      , rupd_expr :: LHsExpr p
      , rupd_flds :: [LHsRecUpdField p]
      }
  ```
- The switch to in-tree annotations introduces a new syntactic element `HsFieldLabel`

  ```
  type family XCHsFieldLabel  x
  type family XXHsFieldLabel  x

  type instance XCHsFieldLabel (GhcPass _) = ApiAnn' AnnFieldLabel

  data AnnFieldLabel
    = AnnFieldLabel {
        afDot :: Maybe AnnAnchor
        } deriving Data

  data HsFieldLabel p
    = HsFieldLabel
      { hflExt   :: XCHsFieldLabel p
      , hflLabel :: Located FieldLabelString
      }
    | XHsFieldLabel !(XXHsFieldLabel p)
  ```
  (see `GHC.Parser.Annotation` for `ApiAnn'`)
- The very abridged detail of in-tree annotations is this:

  - In `Language.Haskell.Syntax.Extension`:

    ```
    type family XRec p a = r | r -> a
    type family Anno a = b -- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
    ```
  - In `GHC.Hs.Extension` type instances:

    ```
    type instance XRec (GhcPass p) a = GenLocated (Anno a) a
    type instance Anno RdrName = SrcSpanAnnN
    type instance Anno Name    = SrcSpanAnnN
    type instance Anno Id      = SrcSpanAnnN
    ```
  - In `Ghc.Hs.Pat` type instances:

    ```
    type instance Anno (HsRecField' p arg) = SrcSpanAnnA
    type instance Anno (HsRecField' (GhcPass p) (LocatedA (HsExpr (GhcPass p)))) = SrcSpanAnnA
    type instance Anno (HsRecField  (GhcPass p) arg) = SrcSpanAnnA

    -- type instance Anno (HsRecUpdField p) = SrcSpanAnnA
    type instance Anno (HsRecField' (AmbiguousFieldOcc p) (LocatedA (HsExpr p))) = SrcSpanAnnA

    type instance Anno (AmbiguousFieldOcc GhcTc) = SrcSpanAnnA
    ```
  (`SrcSpanAnnA` is annotations for items appearing in a list e.g. commas, semiclons).
- In this context, `LHsRecUpdProj` is updated to read:

  ```
  newtype FieldLabelStrings p = FieldLabelStrings [Located (HsFieldLabel p)]
  type RecProj p arg = HsRecField' (FieldLabelStrings p) arg
  type LHsRecProj p arg = XRec p (RecProj p arg)
  type RecUpdProj p = RecProj p (LHsExpr p)
  type LHsRecUpdProj p = XRec p (RecUpdProj p)
  ```
- So, if we were to update the using the same fix strategy as before to work today, we'd:

  - Change `HsFieldLabel` to contain a `RdrName` (not `FastString`)
    ```
    data HsFieldLabel p
     = HsFieldLabel {
          hflExt   :: XCHsFieldLabel p
        , hflLabel :: Located RdrName
       }
     | XHsFieldLabel !(XXHsFieldLabel p)
   ```

  - Then update `AmbiguousFieldOcc` (or whatever replaces it) to contain `LocatedN HsFieldLabel` (not `LocatedN RdrName`):

    ```
    data AmbiguousFieldOcc pass
      = Unambiguous (XUnambiguous pass) (LocatedN HsFieldLabel)
      | Ambiguous   (XAmbiguous pass)   (LocatedN HsFieldLabel)
      | XAmbiguousFieldOcc !(XXAmbiguousFieldOcc pass)
    ```
  - Change `HsRecUpdField` to a `NonEmpty (AmbiguousFieldOcc p)` representation:

    ```
    type HsRecUpdField p = HsRecField' (NonEmpty (AmbiguousFieldOcc p)) (LHsExpr p)
    ```
  - Finally:

    ```
    | RecordUpd
        { rupd_ext  :: XRecordUpd p
        , rupd_expr :: LHsExpr p
        , rupd_flds :: [LHsRecUpdField p]
        }
   ```
--

```
    -- Maybe reuse?
    data FieldOcc pass = FieldOcc { extFieldOcc     :: XCFieldOcc pass
                                , rdrNameFieldOcc :: LocatedN RdrName
                                 -- ^ See Note [Located RdrNames] in "GHC.Hs.Expr"
                                }


   -- We need somewhere to store annotations and maybe a resolved name.
   type Family F
   type instance F (GhcPass GhcPs) = NoExtField
   type instance F (GhcPass GhcRn) = Maybe Name
   type instance XCHsFieldLabel (GhcPass p) = (F p, ApiAnn' AnnFieldLabel)

   -- Then a field label is a located field with that store.
   data HsFieldLabel p
    = HsFieldLabel {
         hflExt   :: XCHsFieldLabel p
       , hflLabel :: Located RdrName
      }
    | XHsFieldLabel !(XXHsFieldLabel p)

    -- Finally we get something like,
    type HsRecUpdField p = HsRecField' (NonEmpty (LocatedN (HsFieldLabel p))) (LHsExpr p)


    -- Adam was musing on...
    data FieldOcc context pass = FIeldOcc { extFieldOcc :: G context pass ,  rdrNameFieldOcc :: Located RdrName }
    type instance G NoDotCtx (GhcPass GhcRn) = Name
```

--
# Representation of field selector occurences
- [Issue](https://gitlab.haskell.org/ghc/ghc/-/issues/19720)
- [MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5598)
- Branch wip/T19720
  ```
  git clean -xdf && git submodule foreach git clean -xdf
  git fetch origin && git merge origin/master
  git checkout -b wip/T19720
  git push origin wip/T19720:wip/T19720
  git submodule update --init --recursive
  ./boot && ./configure && make -j
  ```
- Ghc independent syntax `type family XCFieldOcc x`: `Language/Haskell/Syntax/Extension.hs`
- Ghc independent syntax `type family XXFieldOcc x`: `Language/Haskell/Syntax/Extension.hs`
- Ghc indepedent syntax `data FieldOcc`: `Language/Haskell/Syntax/Type.hs`

- Ghc *dependent* syntax
  - `GHC/Hs/Type.hs`
  ```
  type instance XCFieldOcc GhcPs = NoExtField
  type instance XCFieldOcc GhcRn = Name
  type instance XCFieldOcc GhcTc = Id

  type instance XXFieldOcc (GhcPass _) = NoExtCon
  ```

## First job
- Replace occurrences of:
  - `extFieldOcc` with `foExt`
  - `rdrNameFieldOcc` with `foLabel`
- Ghc independent syntax: `Language/Haskell/Syntax/Expr.hs`: `HsRecFld`
- Annotation refresher
```
{- Values of this type go along with src spans -}
data EpAnn ann
  = EpAnn { entry   :: Anchor
           -- ^ Base location for the start of the syntactic element
           -- holding the annotations.
           , anns     :: ann -- ^ Annotations added by the Parser
           , comments :: EpAnnComments
              -- ^ Comments enclosed in the SrcSpan of the element
              -- this `EpAnn` is attached to
           }
  | EpAnnNotUsed -- ^ No Annotation for generated code,
                  -- e.g. from TH, deriving, etc.
        deriving (Data, Eq, Functor)

{- Pair of an a and a src span -}
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
{- This is the common case; use with EpAnn -}
type SrcAnn ann = SrcSpanAnn' (EpAnn ann)
{- We use EpAnn NameAnn  for names -}
type SrcSpanAnnN = SrcAnn NameAnn
{- EpAnn NameAnn, SrcSpan
type LocatedN = GenLocated SrcSpanAnnN -- (L pair thing)
```
- Renaming in haddock
```
#!/usr/bin/env bash

/usr/bin/find . -name "*.hs" -exec grep -l sed -i'' 's/extFieldOcc/foExt/g' {} \;
/usr/bin/find . -name "*.hs" -exec grep -l sed -i'' 's/rdrNameFieldOcc/foLabel/g' {} \;
/usr/bin/find . -name "*.hs" -exec grep -l sed -i'' 's/HsRecFld/HsRecSel/g' {} \;
/usr/bin/find . -name "*.hs" -exec grep -l sed -i'' 's/XRecFld/XRecSel/g' {} \;
```
- Depends on:
-    - [haddock `wip/T19720`](https://gitlab.haskell.org/ghc/haddock/-/tree/wip/T19720)
-    - Commit SHA: `32e6defa428df872aae7abdc080e67e185dcce87`
- Clean build:
  - `git clean -xdf && git submodule foreach git clean -xdf && git submodule update --init --recursive && ./boot && ./configure && make -j`

--

- Replace
  ```
    instance OutputableBndr (Located (FieldLabelStrings p)) where
      pprInfixOcc = pprInfixOcc . unLoc
      pprPrefixOcc = pprInfixOcc . unLoc

    instance OutputableBndr (FieldLabelStrings p) where
      pprInfixOcc = pprFieldLabelStrings
      pprPrefixOcc = pprFieldLabelStrings
  ```
  with
  ```
  instance (a ~ XRec p (FieldLabelStrings p), Outputable a, UnXRec p) => OutputableBndr a where
    pprInfixOcc = pprInfixOcc . unXRec @p
    pprPrefixOcc = pprPrefixOcc . unXRec @p

  instance {-# OVERLAPPING #-} (UnXRec p, Outputable (XRec p FieldLabelString)) => OutputableBndr (FieldLabelStrings p) where
    pprInfixOcc = pprFieldLabelStrings
    pprPrefixOcc = pprFieldLabelStrings
  ```
