HIE asts are generic trees where `a` carrys type information.

```
data HieAST a =
  Node
    { sourcedNodeInfo :: SourcedNodeInfo a
    , nodeSpan :: Span
    , nodeChildren :: [HieAST a]
    } deriving (Functor, Foldable, Traversable)
```

Information stored in an AST nodes is organized like this.

```
-- | Source of node info
data NodeOrigin
  = SourceInfo
  | GeneratedInfo
    deriving (Eq, Enum, Ord)

-- | NodeInfos grouped by source
newtype SourcedNodeInfo a =
  SourcedNodeInfo { getSourcedNodeInfo :: (M.Map NodeOrigin (NodeInfo a)) }
    deriving (Functor, Foldable, Traversable)

data NodeInfo a = NodeInfo
    { nodeAnnotations :: S.Set (FastString, FastString)
    -- ^ (name of the AST node constructor, name of the AST node Type)

    , nodeType :: [a]
    -- ^ The Haskell types of this node, if any.

    , nodeIdentifiers :: NodeIdentifiers a
    -- ^ All the identifiers and their details
    } deriving (Functor, Foldable, Traversable)

type Identifier = Either ModuleName Name

type NodeIdentifiers a = M.Map Identifier (IdentifierDetails a)

-- | Information associated with every identifier
--
-- We need to include types with identifiers because sometimes multiple
-- identifiers occur in the same span(Overloaded Record Fields and so on)
data IdentifierDetails a = IdentifierDetails
  { identType :: Maybe a
  , identInfo :: S.Set ContextInfo
  } deriving (Eq, Functor, Foldable, Traversable)

-- | Different contexts under which identifiers exist
data ContextInfo
  = Use                -- ^ regular variable
  | MatchBind
  | IEThing IEType     -- ^ import/export
  | TyDecl

  -- | Value binding
  | ValBind
      BindType     -- ^ whether or not the binding is in an instance
      Scope        -- ^ scope over which the value is bound
      (Maybe Span) -- ^ span of entire binding
  -- | Pattern binding
  --
  -- This case is tricky because the bound identifier can be used in two
  -- distinct scopes. Consider the following example (with @-XViewPatterns@)
  --
  -- @
  -- do (b, a, (a -> True)) <- bar
  --    foo a
  -- @
  --
  -- The identifier @a@ has two scopes: in the view pattern @(a -> True)@ and
  -- in the rest of the @do@-block in @foo a@.
  | PatternBind
      Scope        -- ^ scope /in the pattern/ (the variable bound can be used
                   -- further in the pattern)
      Scope        -- ^ rest of the scope outside the pattern
      (Maybe Span) -- ^ span of entire binding

  | ClassTyDecl (Maybe Span)

  -- | Declaration
  | Decl
      DeclType     -- ^ type of declaration
      (Maybe Span) -- ^ span of entire binding

  -- | Type variable
  | TyVarBind Scope TyVarScope

  -- | Record field
  | RecField RecFieldContext (Maybe Span)
  -- | Constraint/Dictionary evidence variable binding
  | EvidenceVarBind
      EvVarSource  -- ^ how did this bind come into being
      Scope        -- ^ scope over which the value is bound
      (Maybe Span) -- ^ span of the binding site

  -- | Usage of evidence variable
  | EvidenceVarUse
    deriving (Eq, Ord)

data RecFieldContext
  = RecFieldDecl
  | RecFieldAssign
  | RecFieldMatch
  | RecFieldOcc
    deriving (Eq, Enum, Ord)
```

Here's the function for node creation.

```
{-# INLINEABLE makeNode #-}
makeNode
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpan                 -- ^ return an empty list if this is unhelpful
  -> ReaderT NodeOrigin m [HieAST b]
makeNode x spn = do
  org <- ask
  pure $ case spn of
    RealSrcSpan span _ -> [Node (mkSourcedNodeInfo org $ simpleNodeInfo cons typ) span []]
    _ -> []
  where
    cons = mkFastString . show . toConstr $ x
    typ = mkFastString . show . typeRepTyCon . typeOf $ x
```
There is a second similar function for when the node has a type to associate with.

```
{-# INLINEABLE makeTypeNode #-}
makeTypeNode
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpan                 -- ^ return an empty list if this is unhelpful
  -> Type                    -- ^ type to associate with the node
  -> ReaderT NodeOrigin m [HieAST Type]
makeTypeNode x spn etyp = do
  org <- ask
  pure $ case spn of
    RealSrcSpan span _ ->
      [Node (mkSourcedNodeInfo org $ NodeInfo (S.singleton (cons,typ)) [etyp] M.empty) span []]
    _ -> []
  where
    cons = mkFastString . show . toConstr $ x
    typ = mkFastString . show . typeRepTyCon . typeOf $ x
```

Building up the `HieM` monad. First, `HieState`.

```
{- Note [Name Remapping]
The Typechecker introduces new names for mono names in AbsBinds.
We don't care about the distinction between mono and poly bindings,
so we replace all occurrences of the mono name with the poly name.
-}
type VarMap a = DVarEnv (Var,a)
data HieState = HieState
  { name_remapping :: NameEnv Id
  , unlocated_ev_binds :: VarMap (S.Set ContextInfo)
  -- These contain evidence bindings that we don't have a location for
  -- These are placed at the top level Node in the HieAST after everything
  -- else has been generated
  -- This includes things like top level evidence bindings.
  }
```

The `Hsc` monad.

```
newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))
    deriving (Functor)
```

`Hsc` is the base monad and `HieM` is,

```
type HieM = ReaderT NodeOrigin (StateT HieState Hsc)
```
The class `ToHie` lifts into `HieM`.

```
class ToHie a where
  toHie :: a -> HieM [HieAST Type]
```

This is the definition for `ConcatM` in effect.

```
concatM :: Monad m => [m [a]] -> m [a]
concatM xs = concat <$> sequence xs
```

Think of `sequence` as being equivalent to
```
  sequence :: Monad m => [m a] -> m [a]
  sequence [] = return []
  sequence (x : xs) =
    x >>= \x ->
    sequence xs >>= \xs ->
    return (x : xs)
```

Here we instantiate `sequence :: [ HieM [HieAst Type] ] -> HieM [[HieAstType]]`, then `fmap concat` in `ConcatM` produces `HieM [HieAstType]`.
