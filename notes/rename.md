# Renaming

Entry point `compile/typecheck/TcRnDriver.hs`, function `tcRnModueTcRnM`. Calls `tcRnSrcDecls` calls `tc_rn_src_decls` calls `rnTopSrcDecls` which calls out to `rnSrcDecls` (defined in `compiler/rename/RnSource.hs`).

`rnSrcDecls` makes a call to `getLocalNonValBinders` in source file `rename/RnNames.hs` where record fields are processed. According to comments, it "gets all the top-level binders bound for a group except for value bindings which are treated separately. Specifically, we return `AvailInfo` for
 * type decls (incl constructors and record selectors)
 * class decls (including class ops)
 * associated types
 * foreign imports
 * value signatures (in hs-boot files only)

```
module Foo where

data R = R { f :: Integer }

g :: R -> Integer
g (R {f = x}) = x
```
`getLocalNonValBinders` first processes type/class decls (except family instances):
```
getLocalNonValBinders fixity_env
     (HsGroup { hs_valds  = binds,
                hs_tyclds = tycl_decls,
                hs_fords  = foreign_decls })
  = do  { -- Process all type/class decls *except* family instances
        ; let inst_decls = tycl_decls >>= group_instds
        ; overload_ok <- xoptM LangExt.DuplicateRecordFields
        ; (tc_avails, tc_fldss)
            <- fmap unzip $ mapM (new_tc overload_ok)
                                 (tyClGroupTyClDecls tycl_decls)
    ...
```
In the example program, `data R = R {f :: Integer}` is the only type decl and so , `new_tc` will be invoked just once.

`new_tc` reads like this
```
    new_tc :: Bool -> LTyClDecl GhcPs
           -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_tc overload_ok tc_decl -- NOT for type/data instances
        = do { let (bndrs, flds) = hsLTyClDeclBinders tc_decl
             ; names@(main_name : sub_names) <- mapM newTopSrcBinder bndrs
             ; flds' <- mapM (newRecordSelector overload_ok sub_names) flds
             ; let fld_env = case unLoc tc_decl of
                     DataDecl { tcdDataDefn = d } -> mk_fld_env d names flds'
                     _                            -> []
             ; return (AvailTC main_name names flds', fld_env) }
```
`hsLTyClDeclBinders tc_decl` returns all the binding names of the decl. The first one is guaranteed to be the name of the decl. The first component represents all binding names except record fields, the second represents field occurences.

The function `newTopSrcBinder` computes a `Name from a `Located RdrName`. `bndrs` then in our case will contain two reader names, the first `R` corresponding to the type `R`, the second `R` corresponding to the constructor `R`. You can see this is in the renamer trace:
```
newTopSrcBinder
  Main
  R
  Reactor1.hs:3:1-27
newTopSrcBinder
  Main
  R
  Reactor1.hs:3:10-27
```
`newGlobalBinder` (`compiler/iface/Ifacenev.hs`) is called on each of these:
```
             do { this_mod <- getModule
                ; traceRn "newTopSrcBinder" (ppr this_mod $$ ppr rdr_name $$ ppr loc)
                ; newGlobalBinder this_mod (rdrNameOcc rdr_name) loc }
```
The effect of `newGlobalBinder` makes a Name for the thing given its Module and OccName. It does this by associating a global unique with the OccName in the Module name cache.
```
{-
*********************************************************
*                                                      *
        Allocating new Names in the Name Cache
*                                                      *
*********************************************************

See Also: Note [The Name Cache] in NameCache
-}
newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
-- See Note [The Name Cache]
--
```
Next up, `flds` are processed
```
             ; flds' <- mapM (newRecordSelector overload_ok sub_names) flds
```
`newRecordSelector` is in `rename/RnNames.hs`. `sub_names` is a list of the data constructor names of the type. Record selector OccNames are built from the underlying field name and the name of the first data constructor of the type to support duplicate record field names. See Note [Why selector names include data constructors].
```
newRecordSelector :: Bool -> [Name] -> LFieldOcc GhcPs -> RnM FieldLabel
... panics
newRecordSelector overload_ok (dc:_) (L loc (FieldOcc _ (L _ fld)))
  = do { selName <- newTopSrcBinder $ L loc $ field
       ; return $ qualFieldLbl { flSelector = selName } }
  where
    fieldOccName = occNameFS $ rdrNameOcc fld
    qualFieldLbl = mkFieldLabelOccs fieldOccName (nameOccName dc) overload_ok
    field | isExact fld = fld
              -- use an Exact RdrName as is to preserve the bindings
              -- of an already renamer-resolved field and its use
              -- sites. This is needed to correctly support record
              -- selectors in Template Haskell. See Note [Binders in
              -- Template Haskell] in Convert.hs and Note [Looking up
              -- Exact RdrNames] in RnEnv.hs.
          | otherwise   = mkRdrUnqual (flSelector qualFieldLbl)
```
`fld` in `FieldOcc _ (L _ fld)` is a record with a `rdrNameFieldOcc` of type `Located RdrName`, `rdrNameOcc fld` produces the OccName from the ReaderName. Remember, OccNames are pairs of namespace and FastString. `occNameFS` extracts the `occNameFS` so `fieldOccName` is the jsut the field label. `mkFieldLabelOccs` is in `basicTypes/FieldLabel.hs`
```
mkFieldLabelOccs :: FieldLabelString -> OccName -> Bool -> FieldLbl OccName
mkFieldLabelOccs lbl dc is_overloaded
  = FieldLabel { flLabel = lbl, flIsOverloaded = is_overloaded
               , flSelector = sel_occ }
  where
    str     = ":" ++ unpackFS lbl ++ ":" ++ occNameString dc
    sel_occ | is_overloaded = mkRecFldSelOcc str
            | otherwise     = mkVarOccFS lbl
```
So the upshot is a `FieldLbl OccName` is created, the `flSelector` field of which is either the `labl or, if overloading is in effect, an OccName in the Var namespace with an occNameFS field that is a formed from the `lbl` and `dc`. In the normal case, the selector of the `FldLbl OccName` is projected out, wrapped into an unqualified RdrName on which `newTopSrcBinder` is invoked adding to the module name cache and computing the corresponding unique Name and finally a FieldLabel is returns with the flSelector field set to that Name. You can see it in the renamer trace here
```
newTopSrcBinder
  Main
  f
  /Users/shaynefletcher/project/da.git/scratch/shayne/no-record-selector-tests/Reactor1.hs:3:14
```
The last bit of `new_tc` looks like this
```
             ; let fld_env = case unLoc tc_decl of
                     DataDecl { tcdDataDefn = d } -> mk_fld_env d names flds'
                     _                            -> []
             ; return (AvailTC main_name names flds', fld_env) }
```
That is, if the `tc_decl` under consideration is a data declaration a `fld_env` is produced from the list of fields produce by the list of FieldLabels `flds' <- mapM (newRecordSelector overload_ok sub_names) flds`.
```
    -- Calculate the mapping from constructor names to fields, which
    -- will go in tcg_field_env. It's convenient to do this here where
    -- we are working with a single datatype definition.
    mk_fld_env :: HsDataDefn GhcPs -> [Name] -> [FieldLabel]
               -> [(Name, [FieldLabel])]
    mk_fld_env d names flds = concatMap find_con_flds (dd_cons d)
      where
      ...
```
Returning attention to `getLocalNonValBinders` type/class decls are done.
```
        ; traceRn "getLocalNonValBinders 1" (ppr tc_avails)
```
For our simple program with the single type, we see the output.
```
getLocalNonValBinders 1 [Main.R{Main.R, Main.R; f}]
```
Interpreting that list works like this : each element is an AvailInfo of case AvailTC. In our case,  their is only one corresponding to the type R. R is the name of the type, which comes first then all of the names harvested from the type : the type, and the field label. The fields are disambiguated from the other type pieces by a ';'.

The AvailInfo type, is defined in `compiler/basicTypes/Avail.hs`
```
-- | Records what things are "available", i.e. in scope
data AvailInfo = Avail Name      -- ^ An ordinary identifier in scope
               | AvailTC Name
                         [Name]
                         [FieldLabel]
                                 -- ^ A type or class in scope. Parameters:
                                 --
                                 --  1) The name of the type or class
                                 --  2) The available pieces of type or class,
                                 --     excluding field selectors.
                                 --  3) The record fields of the type
                                 --     (see Note [Representing fields in AvailInfo]).
                                 --
                                 -- The AvailTC Invariant:
                                 --   * If the type or class is itself
                                 --     to be in scope, it must be
                                 --     *first* in this list.  Thus,
                                 --     typically: @AvailTC Eq [Eq, ==, \/=]@
                deriving( Eq, Data )
                        -- Equality used when deciding if the
                        -- interface has changed

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]
```
The next line of interest.
```
        ; envs <- extendGlobalRdrEnvRn tc_avails fixity_env
```

The type `TcGblEnv` describes the top-level of a module. It is a rather large structure but here the focus is on the embedded `GlobalRdrEnv` (and `FixityEnv`) fields. The GlobalRdrEnv is described as the top-level environment; used during renaming.

The function `extendGlobalRdrEnvRn` updates the GlobalRdrEnv (and the FixityEnv). Assuming no shadowing (not in a TemplateHaskell decl bracket or GHCi), in esscence it reads like this
```
extendGlobalRdrEnvRn avails new_fixities
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; let rdr_env  = tcg_rdr_env gbl_env
              ...
        ; rdr_env2 <- foldM add_gre rdr_env new_gres
        ; let fix_env' = ...
              gbl_env' = gbl_env { tcg_rdr_env = rdr_env2, tcg_fix_env = fix_env' }
        ; traceRn "extendGlobalRdrEnvRn 2" (pprGlobalRdrEnv True rdr_env2)
        ; return (gbl_env', ...) }
    where
     ...
```
So, the workhorses here are `new_gres` and `add_gre`. Before delving in, first the data structures.
```
-- | Global Reader Environment
type GlobalRdrEnv = OccEnv [GlobalRdrElt]
-- ^ Keyed by 'OccName'; when looking up a qualified name
-- we look up the 'OccName' part, and then check the 'Provenance'
-- to see if the appropriate qualification is valid.  This
-- saves routinely doubling the size of the env by adding both
-- qualified and unqualified names to the domain.
--
-- The list in the codomain is required because there may be name clashes
-- These only get reported on lookup, not on construction
--
-- INVARIANT 1: All the members of the list have distinct
--              'gre_name' fields; that is, no duplicate Names
--
-- INVARIANT 2: Imported provenance => Name is an ExternalName
--              However LocalDefs can have an InternalName.  This
--              happens only when type-checking a [d| ... |] Template
--              Haskell quotation; see this note in RnNames
--              Note [Top-level Names in Template Haskell decl quotes]
--
-- INVARIANT 3: If the GlobalRdrEnv maps [occ -> gre], then
--                 greOccName gre = occ
--
--              NB: greOccName gre is usually the same as
--                  nameOccName (gre_name gre), but not always in the
--                  case of record seectors; see greOccName

-- | Global Reader Element
--
-- An element of the 'GlobalRdrEnv'
data GlobalRdrElt
  = GRE { gre_name :: Name
        , gre_par  :: Parent
        , gre_lcl :: Bool          -- ^ True <=> the thing was defined locally
        , gre_imp :: [ImportSpec]  -- ^ In scope through these imports
    } deriving (Data, Eq)
         -- INVARIANT: either gre_lcl = True or gre_imp is non-empty
         -- See Note [GlobalRdrElt provenance]
```
So, a GlobalRdrEnv is maps OccNames to a alist of GlobalRdrElt.  `new_gres` computes a list of locally defined GlobalRdrElts from the provided list of AvailInfo.

```
    new_gres :: [GlobalRdrElt]  -- New LocalDef GREs, derived from avails
    new_gres = concatMap localGREsFromAvail avails
```

The gist of `localGREsFromAvail` (`compiler/basicTypes/RdrName.hs`) is `gresFromAvail` with provenance set to `Nothing` (here I mean since our names are defined in the module, not imported) which is itself a concatMap of gresFromAvail over avails.
```
gresFromAvail :: (Name -> Maybe ImportSpec) -> AvailInfo -> [GlobalRdrElt]
gresFromAvail prov_fn avail
  = map mk_gre (availNonFldNames avail) ++ map mk_fld_gre (availFlds avail)
  where
    mk_gre n
      = case prov_fn n of  -- Nothing => bound locally
                           -- Just is => imported from 'is'
          Nothing -> GRE { gre_name = n, gre_par = mkParent n avail
                         , gre_lcl = True, gre_imp = [] }
          Just is -> GRE { gre_name = n, gre_par = mkParent n avail
                         , gre_lcl = False, gre_imp = [is] }

    mk_fld_gre (FieldLabel { flLabel = lbl, flIsOverloaded = is_overloaded
                           , flSelector = n })
      = case prov_fn n of  -- Nothing => bound locally
                           -- Just is => imported from 'is'
          Nothing -> GRE { gre_name = n, gre_par = FldParent (availName avail) mb_lbl
                         , gre_lcl = True, gre_imp = [] }
          Just is -> GRE { gre_name = n, gre_par = FldParent (availName avail) mb_lbl
                         , gre_lcl = False, gre_imp = [is] }
      where
        mb_lbl | is_overloaded = Just lbl
               | otherwise     = Nothing
```
`availNonFldNames` and `availFlds` produce lists of names for non-fields and then fields respectively. `makeParent` is defined
```
mkParent :: Name -> AvailInfo -> Parent
mkParent _ (Avail _)           = NoParent
mkParent n (AvailTC m _ _) | n == m    = NoParent
                         | otherwise = ParentIs m
```
and `availName` is just the main name of the available (that is the type or class).
```
-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName (Avail n)     = n
availName (AvailTC n _ _) = n
```
The purpose of `add_gre` is teo extend a `GlobalRdrEnv` with a `GlobalRdrElt`.
```
    add_gre env gre
      | not (null dups)    -- Same OccName defined twice
      = do { addDupDeclErr (gre : dups); return env }

      | otherwise
      = return (extendGlobalRdrEnv env gre)
      where
        name = gre_name gre
        occ  = nameOccName name
        dups = filter isLocalGRE (lookupGlobalRdrEnv env occ)
```

## Value binding


compiler/basicTypes/BasicTypes.hs

data TopLevelFlag
  = TopLevel
  | NotTopLevel

compiler/rename/RnFixity.hs

type MiniFixityEnv = FastStringEnv (Located Fixity)
        -- Mini fixity env for the names we're about
        -- to bind, in a single binding group
        --
        -- It is keyed by the *FastString*, not the *OccName*, because
        -- the single fixity decl       infix 3 T
        -- affects both the data constructor T and the type constrctor T
        --
        -- We keep the location so that if we find
        -- a duplicate, we can report it sensibly

compiler/rename/RnPat.hs

*********************************************************
*                                                      *
        Name makers
*                                                      *
*********************************************************

Externally abstract type of name makers,
which is how you go from a RdrName to a Name
-}


data NameMaker
  = LamMk       -- Lambdas
      Bool      -- True <=> report unused bindings
                --   (even if True, the warning only comes out
                --    if -Wunused-matches is on)

  | LetMk       -- Let bindings, incl top level
                -- Do *not* check for unused bindings
      TopLevelFlag
      MiniFixityEnv

topRecNameMaker :: MiniFixityEnv -> NameMaker
topRecNameMaker fix_env = LetMk TopLevel fix_env


-- for top-level bindings, we need to make top-level names,
-- so we have a different entry point than for local bindings
rnTopBindsLHS :: MiniFixityEnv
              -> HsValBinds GhcPs
              -> RnM (HsValBindsLR GhcRn GhcPs)
rnTopBindsLHS fix_env binds
  = rnValBindsLHS (topRecNameMaker fix_env) binds

-- renames the left-hand sides
-- generic version used both at the top level and for local binds
-- does some error checking, but not what gets done elsewhere at the top level
rnValBindsLHS :: NameMaker
              -> HsValBinds GhcPs
              -> RnM (HsValBindsLR GhcRn GhcPs)
rnValBindsLHS topP (ValBinds x mbinds sigs)
  = do { mbinds' <- mapBagM (wrapLocM (rnBindLHS topP doc)) mbinds
       ; return $ ValBinds x mbinds' sigs }
  where
    bndrs = collectHsBindsBinders mbinds
    doc   = text "In the binding group for:" <+> pprWithCommas ppr bndrs

rnBindLHS :: NameMaker
          -> SDoc
          -> HsBind GhcPs
          -- returns the renamed left-hand side,
          -- and the FreeVars *of the LHS*
          -- (i.e., any free variables of the pattern)
          -> RnM (HsBindLR GhcRn GhcPs)

rnBindLHS name_maker _ bind@(FunBind { fun_id = rdr_name })
  = do { name <- applyNameMaker name_maker rdr_name
       ; return (bind { fun_id = name
                      , fun_ext = noExt }) }

applyNameMaker :: NameMaker -> Located RdrName -> RnM (Located Name)
applyNameMaker mk rdr = do { (n, _fvs) <- runCps (newPatLName mk rdr)
                           ; return n }

newPatName (LetMk is_top fix_env) rdr_name
  = CpsRn (\ thing_inside ->
        do { name <- case is_top of
                       NotTopLevel -> newLocalBndrRn rdr_name
                       TopLevel    -> newTopSrcBinder rdr_name
           ; bindLocalNames [name] $       -- Do *not* use bindLocalNameFV here
                                        -- See Note [View pattern usage]
             addLocalFixities fix_env [name] $
             thing_inside name })
