{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Lens.Barlow.Classes where

import Data.Data (Proxy (..))
import Data.Lens.Barlow.Parser (Parse)
import Data.Lens.Barlow.Types
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, symbolVal)
import GHC.TypeNats (natVal)

class KnownTag (a :: Tag) where
  tagVal :: TagVal

instance KnownTag Tag'QuestionMark where tagVal = TagVal'QuestionMark
instance KnownTag Tag'RightArrow where tagVal = TagVal'RightArrow
instance KnownTag Tag'LeftArrow where tagVal = TagVal'LeftArrow
instance KnownTag Tag'Plus where tagVal = TagVal'Plus
instance KnownTag Tag'ExclamationMark where tagVal = TagVal'ExclamationMark
instance (KnownSymbol a) => KnownTag (Tag'PercentageName a) where tagVal = TagVal'PercentageName (symbolVal (Proxy @a))
instance (KnownNat a) => KnownTag (Tag'PercentageNumber a) where tagVal = TagVal'PercentageNumber (natVal (Proxy @a))
instance (KnownSymbol a) => KnownTag (Tag'Name a) where tagVal = TagVal'Name (symbolVal (Proxy @a))

class KnownTags (a :: [Tag]) where
  tagVals :: [TagVal]

instance KnownTags '[] where
  tagVals = []

instance (KnownTag x, KnownTags xs) => KnownTags (x : xs) where
  tagVals = (tagVal @x) : tagVals @xs

class KnownSymbolTags (s :: Symbol) where
  symbolTagVals :: [TagVal]

instance (KnownTags (Parse s)) => KnownSymbolTags s where
  symbolTagVals = tagVals @(Parse s)
