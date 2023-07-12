{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.Lens.Barlow.Parser where

import Data.Data (Proxy)
import GHC.Base (Constraint, Symbol)

-- Translating https://github.com/sigma-andex/purescript-barlow-lens/blob/main/src/Data/Lens/Barlow/Parser.purs

-------------------------
-- Closed Type Families approach
-------------------------

-- type family ParsePercentage (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) :: Constraint where

-- type family ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) :: (Symbol, Symbol) where
--   ParsePercentageSymbol "." t = '("", t)
--   ParsePercentageSymbol "" t = '("", t)
--   ParsePercentageSymbol t "" = '(t, "")
--   ParsePercentageSymbol p s = '("a", "b")

-----------------------
-- Classes approach
-----------------------
-- class ParsePercentage (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) | head tail -> out rest

-- class ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

-- instance ParsePercentageSymbol "." t "" t
-- instance ParsePercentageSymbol " " t "" t
-- instance ParsePercentageSymbol t "" t ""

-----------------------
-- Classes + associated type families approach
-----------------------
-- class ParsePercentage (head :: Symbol) (tail :: Symbol) where
--   type P head tail :: (Symbol, Symbol)

-- class ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) where
--   type PS head tail :: (Symbol, Symbol)

-- instance ParsePercentageSymbol "." t where type PS "." t = '("", t)
-- instance ParsePercentageSymbol " " t where type PS " " t = '("", t)
-- instance ParsePercentageSymbol t "" where type PS t "" = '(t, "")