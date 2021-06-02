# Annotations in GHC

Starting with ghc-9.2.1, parse trees contain "annotations" (these are, for example, comments and the locations of keywords). This represents a non-trivial upgrade of GHC parse trees. If you work with GHC ASTs in your project, there will be no avoiding getting to know about them. This note is a summary overview of annotations: the where and how of their representations.

In-tree annotations enable exact-printing of GHC ASTs. This feature and the reformulation of the GHC AST with in-tree annotations to support it was conceived of and implemented by Alan Zimmerman (@alan_zimm). The achievement is of truly epic scale.

## Annotations on syntactic elements

An `EpaLocation` is a span giving the exact location of a keyword in parsed source.
```haskell
data EpaLocation = EpaSpan RealSrcSpan | EpaDelta DeltaPos
data DeltaPos = ...
```
The parser only inserts `EpaSpans`.

A `DotFieldOcc` arises in expressions like `(.e)` (field-selector) or `a.e` (field-selection) when `OverloadedRecordDot` is enabled. A `DotFieldOcc` value in the parse phase is associated with an `AnnFieldLabel` in its extension field (annotations in ghc-9.2.1 lean heavily on the facilities afforded by [TTG](https://arxiv.org/abs/1610.04799)). The `AnnFieldLabel` contains the location of the '`.`'. `AnnFieldLabel` is an "annotation type". You'll recognize annotation types (there are many) by the convention that their names are prefixed `Ann`.
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
 (*What `XRec p FieldLabelString` means will be explained in the next section.*)

Note that the extension field `dfoExt` doesn't contain a "raw" `AnnFieldLabel`, rather, it contains an `EpAnn AnnFieldLabel`.

`EPAnn`, envelopes an annotation. It associates a base location for the start of the syntactic element containing the annotation along with any comments enclosed in the source span of the element to which the `EPAnn` is attached. `EpAnnUnsed` is used when an annotation is required but there's no annotation available to envelope (e.g one obvious case being in generated code).
```haskell
data EpAnn ann
  = EpAnn { entry :: Anchor
          , anns :: ann
          , comments :: EpAnnComments }
  | EpAnnNotUsed

data EpAnnComments = ...
```
It's the `Anchor` type where the base location is held.
```haskell
data Anchor = Anchor { anchor :: RealSrcSpan, anchor_op :: AnchorOperation }

data AnchorOperator = ...
```

## Annotations on source spans

Annotations don't just get attached to syntactic elements, they frequently get attached to source spans too.
```haskell
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
```
Usually `SrcSpanAnn'` is used with `EpAnn` and that combination is named a `SrcAnn`.
```haskell
data SrcAnn ann = SrcSpanAnn' (EpAnn ann)
```

There are many annotation types. The most ubiquitous are `AnnListItem`, `NameAnn`, `AnnList`, `AnnPragma` and `AnnContext`. Their use is common enough that names are given to their `SrcAnn` types (which you recall, wrap them in `EpAnn` and associate them with a `SrcSpan`).
```haskell
type SrcSpanAnnA = SrcAnn AnnListItem
type SrcSpanAnnN = SrcAnn NameAnn

type SrcSpanAnnL = SrcAnn AnnList
type SrcSpanAnnP = SrcAnn AnnPragma
type SrcSpanAnnC = SrcAnn AnnContext
```
Of these, `SrcSpanAnnA` is used as a sort of "default" annotation.

What do you do with generalized `SrcSpan` types like these? You locate things with them.
```haskell
type LocatedA = GenLocated SrcSpanAnnA
type LocatedN = GenLocated SrcSpanAnnN

type LocatedL = GenLocated SrcSpanAnnL
type LocatedP = GenLocated SrcSpanAnnP
type LocatedC = GenLocated SrcSpanAnnC
```
These type synonyms are only for the most commonly used annoation types. The general case is `LocatedAn an`.
```haskell
type LocatedAn an = GenLocated (SrcAnn an)
```
To recap, a `LocatedAn an` is a `GenLocated (SrcAnn an)` which is a `GenLocated (SrcSpanAnn' (EpAnn an))`.

## Abstracting over locations

Syntax definitions are generalized with respect to location information. That is, rather than hard-coding `SrcSpan` into syntax type definitions as we used to, type families are used in their place so that the structure of the syntax including locations can be described without fixing concrete types for the locations where you'd once have had a source span type.

It works like this. In `Language.Haskell.Syntax.Extension` there is this definition:
```haskell
type family XRec p a = r | r -> a
```
Locations are specified in terms of `XRec`s. For example in `Language.Haskell.Syntax.Expr` we have this:
```haskell
type LHsExpr p = XRec p (HsExpr p)
```
How `XRec p (HsExpr p)` is mapped onto a specific type in GHC is achieved in the following way. First in `Language.Haskell.Syntax.Extension` there is the following definition:
```haskell
type family Anno a = b
```
Then, in `GHC.Hs.Extension` this definition:
```haskell
type instance XRec (GhcPass p) a = GenLocated (Anno a) a
```

Specific choices for each syntatic element can then be made for GHC's use of the parse tree and phase. For example, in `GHC.Hs.Expr` we have the following.
```haskell
type instance Anno (HsExpr (GhcPass pass)) = SrcSpanAnnA
```
To see how this works, consider what that means for the located expression type `LHsExpr GhcPs` in GHC. We have `LHsExpr GhcPs` is `XRec GhcPs (HsExpr GhcPs)` which is `GenLocated (Anno (HsExpr GhcPs)) (HsExpr GhcPs)` or `GenLocated SrcSpanAnnA (HsExpr GhcPs)` (or, `LocatedA (HsExpr GhcPs))` if you like).

Expanding further we have `GenLocated SrcSpanAnnA (HsExpr GhcPs)` is `GenLocated (SrcAnn AnnListItem) (HsExpr GhcPs)`. So ultimately, `LHsExpr GhcPs` is `GenLocated (SrcSpanAnn' (EpAnn AnnListItem)) (HsExpr GhcPs)`.
