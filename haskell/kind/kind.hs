{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Kind

type Id :: k -> k  -- kind signature: k is a kind var
type Id (x :: k) = x  -- definition of `Id`

type Foo :: Bool
type Foo = Id @Bool False -- `Foo` is type `False` of kind `Bool`

main = undefined
