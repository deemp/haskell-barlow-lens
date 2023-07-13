{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Lens.Barlow.ParserClasses () where

import Data.Lens.Barlow.Types
import GHC.Base (Symbol)
import GHC.TypeLits (AppendSymbol)

-- Translating https://github.com/sigma-andex/purescript-barlow-lens/blob/main/src/Data/Lens/Barlow/Parser.purs

-------------------------
-- Tagged classes approach
-------------------------

data TagPS
  = TagPS'Dot
  | TagPS'Space
  | TagPS'End
  | TagPS'Cons

type family SelectTagPS (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) :: TagPS where
  SelectTagPS "." t "." t = TagPS'Dot
  SelectTagPS " " t "" t = TagPS'Space
  SelectTagPS h "" h "" = TagPS'End
  SelectTagPS _ _ _ _ = TagPS'Cons

class ParsePercentageSymbol' (tag :: TagPS) (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol)

instance ParsePercentageSymbol' TagPS'Dot "." t "" t
instance ParsePercentageSymbol' TagPS'Space " " t "" t
instance ParsePercentageSymbol' TagPS'End h "" h ""
instance (t ~ AppendSymbol th tt, ParsePercentageSymbol th tt tout trest, out ~ AppendSymbol h tout) => ParsePercentageSymbol' TagPS'Cons h t out trest

type ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) =
  ParsePercentageSymbol' (SelectTagPS head tail out rest) head tail out rest

--

data TagP
  = TagP'1
  | TagP'2
  | TagP'3
  | TagP'4
  | TagP'5
  | TagP'6
  | TagP'7
  | TagP'8
  | TagP'9
  | TagP'End
  | TagP'Sym

type family SelectTagP (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) :: TagP where
  SelectTagP "1" t N1 t = TagP'1
  SelectTagP "2" t N2 t = TagP'2
  SelectTagP "3" t N3 t = TagP'3
  SelectTagP "4" t N4 t = TagP'4
  SelectTagP "5" t N5 t = TagP'5
  SelectTagP "6" t N6 t = TagP'6
  SelectTagP "7" t N7 t = TagP'7
  SelectTagP "8" t N8 t = TagP'8
  SelectTagP "9" t N9 t = TagP'9
  SelectTagP t "" h "" = TagP'End
  SelectTagP _ _ _ _ = TagP'Sym

class ParsePercentage' (tag :: TagP) (head :: Symbol) (tail :: Symbol) (out :: k) (rest :: Symbol)

instance ParsePercentage' TagP'1 "1" t N1 t
instance ParsePercentage' TagP'2 "2" t N2 t
instance ParsePercentage' TagP'3 "3" t N3 t
instance ParsePercentage' TagP'4 "4" t N4 t
instance ParsePercentage' TagP'5 "5" t N5 t
instance ParsePercentage' TagP'6 "6" t N6 t
instance ParsePercentage' TagP'7 "7" t N7 t
instance ParsePercentage' TagP'8 "8" t N8 t
instance ParsePercentage' TagP'9 "9" t N9 t
instance ParsePercentage' TagP'End t "" h ""
instance (t ~ AppendSymbol th tt, ParsePercentageSymbol th tt tout trest, out ~ AppendSymbol h tout) => ParsePercentage' TagP'Sym h t out trest

type ParsePercentage (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) =
  ParsePercentage' (SelectTagP head tail out rest) head tail out rest

--

data TagRF
  = TagRF'Dot
  | TagRF'QuestionMark
  | TagRF'LeftArrow
  | TagRF'RightArrow
  | TagRF'Plus
  | TagRF'ExclamationMark
  | TagRF'End
  | TagRF'Cons

type family SelectRFTag (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) :: TagRF where
  SelectRFTag "." t "" t = TagRF'Dot
  SelectRFTag "?" t "" out = TagRF'QuestionMark
  SelectRFTag "<" t "" out = TagRF'LeftArrow
  SelectRFTag ">" t "" out = TagRF'RightArrow
  SelectRFTag "+" t "" out = TagRF'Plus
  SelectRFTag "!" t "" out = TagRF'ExclamationMark
  SelectRFTag h "" h "" = TagRF'End
  SelectRFTag _ _ _ _ = TagRF'Cons

class ParseRecordField' (tag :: TagRF) (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol)

instance ParseRecordField' TagRF'Dot "." t "" t
instance (out ~ AppendSymbol "?" t) => ParseRecordField' TagRF'QuestionMark "?" t "" out
instance (out ~ AppendSymbol "<" t) => ParseRecordField' TagRF'LeftArrow "<" t "" out
instance (out ~ AppendSymbol ">" t) => ParseRecordField' TagRF'RightArrow ">" t "" out
instance (out ~ AppendSymbol "+" t) => ParseRecordField' TagRF'Plus "+" t "" out
instance (out ~ AppendSymbol "!" t) => ParseRecordField' TagRF'ExclamationMark "!" t "" out
instance ParseRecordField' TagRF'End h "" h ""
instance (t ~ AppendSymbol th tt, ParseRecordField th tt tout trest, out ~ AppendSymbol h tout) => ParseRecordField' TagRF'Cons h t out trest

type ParseRecordField (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) =
  ParseRecordField' (SelectRFTag head tail out rest) head tail out rest

--

data Tag1S
  = Tag1S'Nil
  | Tag1S'Dot
  | Tag1S'QuestionMark
  | Tag1S'RightArrow
  | Tag1S'LeftArrow
  | Tag1S'Plus
  | Tag1S'ExclamationMark
  | Tag1S'Percentage
  | Tag1S'Else

type family SelectTag1S (head :: Symbol) (tail :: Symbol) (out :: TList) :: Tag1S where
  SelectTag1S a "" (TCons (RecordField a) TNil) = Tag1S'Nil
  SelectTag1S "." s rest = Tag1S'Dot
  SelectTag1S "?" s (TCons QuestionMark rest) = Tag1S'QuestionMark
  SelectTag1S ">" s (TCons RightArrow rest) = Tag1S'RightArrow
  SelectTag1S "<" s (TCons LeftArrow rest) = Tag1S'LeftArrow
  SelectTag1S "+" s (TCons Plus rest) = Tag1S'Plus
  SelectTag1S "!" s (TCons ExclamationMark rest) = Tag1S'ExclamationMark
  SelectTag1S "%" s (TCons Percentage rest) = Tag1S'Percentage
  SelectTag1S h t (TCons (RecordField out) rest) = Tag1S'Else

class Parse1Symbol' (tag :: Tag1S) (head :: Symbol) (tail :: Symbol) (out :: TList)
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'Dot "." s rest
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'QuestionMark "?" s (TCons QuestionMark rest)
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'RightArrow ">" s (TCons RightArrow rest)
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'LeftArrow "<" s (TCons LeftArrow rest)
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'Plus "+" s (TCons Plus rest)
instance (ParseSymbol s rest) => Parse1Symbol' Tag1S'ExclamationMark "!" s (TCons ExclamationMark rest)
instance (t ~ AppendSymbol th tt, ParsePercentage th tt tout trest, ParseSymbol trest rest) => Parse1Symbol' Tag1S'Percentage "%" s (TCons Percentage rest)
instance (t ~ AppendSymbol th tt, ParseRecordField th tt tout trest, out ~ AppendSymbol h tout, ParseSymbol trest rest) => Parse1Symbol' Tag1S'Else h t (TCons (RecordField out) rest)

type Parse1Symbol (head :: Symbol) (tail :: Symbol) (out :: TList) =
  Parse1Symbol' (SelectTag1S head tail out) head tail out

--

data TagS
  = TagS'QuestionMark
  | TagS'RightArrow
  | TagS'LeftArrow
  | TagS'Plus
  | TagS'ExclamationMark
  | TagS'Nil
  | TagS'Cons

type family SelectTagS (string :: Symbol) (attributes :: TList) :: TagS where
  SelectTagS "?" (TCons QuestionMark TNil) = TagS'QuestionMark
  SelectTagS ">" (TCons RightArrow TNil) = TagS'RightArrow
  SelectTagS "<" (TCons LeftArrow TNil) = TagS'LeftArrow
  SelectTagS "+" (TCons Plus TNil) = TagS'Plus
  SelectTagS "!" (TCons ExclamationMark TNil) = TagS'ExclamationMark
  SelectTagS "" TNil = TagS'Nil
  SelectTagS string fl = TagS'Cons

class ParseSymbol' (tag :: TagS) (string :: Symbol) (attributes :: TList)
instance ParseSymbol' TagS'QuestionMark "?" (TCons QuestionMark TNil)
instance ParseSymbol' TagS'RightArrow ">" (TCons RightArrow TNil)
instance ParseSymbol' TagS'LeftArrow "<" (TCons LeftArrow TNil)
instance ParseSymbol' TagS'Plus "+" (TCons Plus TNil)
instance ParseSymbol' TagS'ExclamationMark "!" (TCons ExclamationMark TNil)
instance ParseSymbol' TagS'Nil "" TNil
instance (string ~ AppendSymbol h t, Parse1Symbol h t fl) => ParseSymbol' TagS'Cons string fl

type ParseSymbol (string :: Symbol) (attributes :: TList) =
  ParseSymbol' (SelectTagS string attributes) string attributes

ex :: ParseSymbol "?" TNil => Int
ex = 3

-- >>>:t ex
-- Couldn't match type `AppendSymbol h0_a6BKv[tau:1] t0_a6BKw[tau:1]'
--                with `"?"'
--   arising from a use of `ex'
-- The type variables `h0_a6BKv[tau:1]',
--                    `t0_a6BKw[tau:1]' are ambiguous
-- In the expression: ex

-- type P a = ParseSymbol "?" a => Proxy a

-- >>>:kind! P TNil
-- P TNil :: *
-- = ParseSymbol' (SelectTagS "?" 'TNil) "?" 'TNil => Proxy 'TNil

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

-- class C a b
-- instance C Int Int
-- instance C Int a
-- instance C a Int

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

----------------------
-- Parsing using TF
----------------------
