# Annotations in GHC

Starting with ghc-9.2.1, parse trees contain "annotations" (these are, for example, comments and the locations of keywords). This note is a summary overview of where and how they're represented.

## Annotations on syntactic elements

An `EpaLocation` is a span giving the exact location of a keyword in parsed source.
```haskell
data EpaLocation = EpaSpan RealSrcSpan | EpaDelta DeltaPos
data DeltaPos = ...
```
The parser only inserts `EpaSpans`.

A `DotFieldOcc` arises in expressions like `(.e)` (field-selector) or `a.e` (field-selection) when `OverloadedRecordDot` is enabled. A `DotFieldOcc` value in the parse phase is associated with a `AnnFieldLabel` in its extension field. The annotation contains the location of the '`.`'. `AnnFieldLabel` is an "annotation type".
```haskell
-- GHC.Hs.Expr
data AnnFieldLabel
  = AnnFieldLabel {
      afDot :: Maybe EpaLocation
      }
type instance XCDotFieldOcc (GhcPass _) = EpAnn AnnFieldLabel

-- Language.Haskell.Syntax.Expr
data DotFieldOcc p
  = DotFieldOcc
    { dfoExt   :: XCDotFieldOcc p
    , dfoLabel :: XRec p FieldLabelString
    }
  | XDotFieldOcc !(XXDotFieldOcc p)
```
Note that the extension field doesn't contain a "raw" `AnnFieldLabel`, rather it contains an `EpAnn AnnFieldLabel`.

`EPAnn`, envelopes an annotation. It associates a base location for the start of the syntactic element containing the annotation along with any comments enclosed in the src span of the element to which the `EPAnn` is attached. `EpAnnUnsed` is used when an annotation is required but there's no annotation available to envelope (e.g one obvious case being in generated code).
```haskell
data EpAnn ann
  = EpAnn { entry :: Anchor
          , anns :: ann
          , comments :: EpAnnComments }
  | EpAnnNotUsed

data EpAnnComments = ...
```
The type `Anchor` holds the base location.
```
data Anchor = Anchor { anchor :: RealSrcSpan, anchor_op :: AnchorOperation }

data AnchorOperator = ...
```

## Annotations on source spans

Annotations don't just get attached to syntactic elements in their extension fields, they frequently get attached to source spans too.
```
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
```
Usually `SrcSpanAnn'` is used with `EpAnn` and that combination is named a `SrcAnn`.
```
data SrcAnn ann = SrcSpanAnn' (EpAnn ann)
```

There are many annotation types. The most ubiquitous are `AnnListItem`, `NameAnn`, `AnnList`, `AnnPragma` and `AnnContext`. They are so ubiquitous that names are given to their `SrcAnn` types (which you recall, wrap them in `EpAnn` and associate them with a `SrcSpan`).
```
type SrcSpanAnnA = SrcAnn AnnListItem
type SrcSpanAnnN = SrcAnn NameAnn

type SrcSpanAnnL = SrcAnn AnnList
type SrcSpanAnnP = SrcAnn AnnPragma
type SrcSpanAnnC = SrcAnn AnnContext
```
Of these, `SrcSpanAnnA` is used as a sort of "default" annotation.

What do you do with generalized `SrcSpan` types like these? You locate things with them.
```
type LocatedA = GenLocated SrcSpanAnnA
type LocatedN = GenLocated SrcSpanAnnN

type LocatedL = GenLocated SrcSpanAnnL
type LocatedP = GenLocated SrcSpanAnnP
type LocatedC = GenLocated SrcSpanAnnC
```
These type synonyms are only for the most commonly used annoation types. The general case is `LocatedAn an`.
```
type LocatedAn an = GenLocated (SrcAnn an)
```
To recap, a `LocatedAn an` is a `GenLocated (SrcAnn ann)` which is a `GenLocated (SrcSpanAnn' (EpAnn ann))`.

## Abstracting over locations

Syntax definitions are generalized with respect to location information. That is, rather than hard-coding `SrcSpan` into syntax type definitions as we used to, type families are used in their place so that the structure of the syntax including locations can be described without fixing concrete types for the locations where you'd once have had a source span type.

It work's like this. In `Language.Haskell.Syntax.Extension` there is this definition.
```
type family XRec p a = r | r -> a
```
Locations are specified in terms of `XRec`s. For example:
```
type LHsExpr p = XRec p (HsExpr p)
```
How `XRec p (HsExpr p)` is mapped onto a specific type in GHC is achieved in the following way. First in `Language.Haskell.Syntax.Extension` there is the following definition.
```
type family Anno a = b
```
Then, in `GHC.Hs.Extension` this definition.
```
type instance XRec (GhcPass p) a = GenLocated (Anno a) a
```

Specific choices for each syntatic element can then be made for GHC's use of the AST. For example, in `GHC.Hs.Expr` we have the following.
```haskell
type instance Anno (HsExpr (GhcPass P)) = SrcSpanAnnA
```
To see how this works, consider what that means for the located expression type `LHsExpr GhcPs` in GHC. We have `LHsExpr GhcPs` is `XRec GhcPs (HsExpr GhcPs)` which is `GenLocated (Anno (HsExpr GhcPs)) (HsExpr GhcPs)` or `GenLocated SrcSpanAnnA (HsExpr GhcPs)` (or, `LocatedA (HsExpr GhcPs))` if you like).

Expanding further we have `GenLocated SrcSpanAnnA (HsExpr GhcPs)` is `GenLocated (SrcAnn AnnListItem) (HsExpr GhcPs)`. So ultimately, `LHsExpr GhcPs` is `GenLocated (SrcSpanAnn' (EpAnn AnnListItem)) (HsExpr GhcPs)`.
