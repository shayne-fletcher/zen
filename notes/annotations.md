First we have in `Language.Haskell.Syntax.Extension` the definition,
```
-- | GHC's L prefixed variants wrap their vanilla variant in this type family,
-- to add 'SrcLoc' info via 'Located'. Other passes than 'GhcPass' not
-- interested in location information can define this as
-- @type instance XRec NoLocated a = a@.
-- See Note [XRec and SrcSpans in the AST]
type family XRec p a = r | r -> a

type family Anno a = b -- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
```
`GHC.Hs.Extension` supplies type instances for GHC passes:
```
-- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
type instance XRec (GhcPass p) a = GenLocated (Anno a) a

type instance Anno RdrName = SrcSpanAnnN
type instance Anno Name    = SrcSpanAnnN
type instance Anno Id      = SrcSpanAnnN
```
`GHC.Parser.Annotation` is where we find the definition of `SrcSpanAnnN`:
```
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
type ApiAnn = ApiAnn' [AddApiAnn]
type SrcAnn ann = SrcSpanAnn' (ApiAnn' ann)
data NameAnn
  -- | Used for a name with an adornment, so '`foo`', '(bar)'
  = NameAnn {...} | NameAnnCommas {...} | ...
type SrcSpanAnnN = SrcAnn NameAnn
```
