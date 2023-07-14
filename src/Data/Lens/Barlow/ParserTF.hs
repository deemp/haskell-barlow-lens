{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Lens.Barlow.ParserTF (Parse) where

-- Translating https://github.com/sigma-andex/purescript-barlow-lens/blob/main/src/Data/Lens/Barlow/Parser.purs

import Data.Data (Proxy (Proxy))
import Data.Lens.Barlow.Types
import Fcf (Eval, If)
import Fcf.Data.List as DL (Elem, Reverse)
import GHC.TypeLits

import Fcf.Class.Foldable (Or)
import GHC.TypeLits qualified as TL

type family FromChars1 (cs :: [Char]) (res :: Symbol) :: Symbol where
  FromChars1 '[] res = res
  FromChars1 (c : cs) res = FromChars1 cs (ConsSymbol c res)

type family FromCharsReverse (cs :: [Char]) :: Symbol where
  FromCharsReverse cs = FromChars1 cs ""

type family FromChars (cs :: [Char]) :: Symbol where
  FromChars cs = FromChars1 (Eval (Reverse cs)) ""

type family ToChars1 (s :: Maybe (Char, Symbol)) (r :: [Char]) :: [Char] where
  ToChars1 Nothing s = Eval (Reverse s)
  ToChars1 ('Just '(c, cs)) s = ToChars1 (UnconsSymbol cs) (c ': s)

type family ToChars (s :: Symbol) :: [Char] where
  ToChars s = ToChars1 (UnconsSymbol s) '[]

type family FromChar (c :: Char) :: Symbol where
  FromChar c = FromChars '[c]

type family AppendChar (s :: Symbol) (c :: Char) :: Symbol where
  AppendChar s c = AppendSymbol s (FromChar c)

type family CharBetween1 (c1 :: Ordering) (c2 :: Ordering) :: Bool where
  CharBetween1 EQ LT = True
  CharBetween1 LT EQ = True
  CharBetween1 LT LT = True
  CharBetween1 a b = False

type family CharBetween (c :: Char) (lowerBound :: Char) (upperBound :: Char) :: Bool where
  CharBetween c lowerBound upperBound = CharBetween1 (CmpChar lowerBound c) (CmpChar c upperBound)

-- TODO remove constraint

type SpecialChars = '[ '.', '?', '>', '<', '+', '!', '%']

type IsSpecial (x :: Char) = Eval (Elem x SpecialChars)

type DigitNat d = (CharToNat d - CharToNat '0')

type family UnexpectedCharacterError (c :: Char) (expected :: Symbol) (prefix :: [Char]) (rest :: [Char]) :: k where
  UnexpectedCharacterError c expected prefix rest =
    TypeError
      ( (Text "Unexpected character: " :<>: Text (FromChar c) :<>: Text "\n")
          :<>: (Text expected :<>: Text "\n")
          :<>: (Text "in " :<>: Text (FromCharsReverse prefix) :<>: Text "\n")
          :<>: (Text "in " :<>: Text (AppendSymbol (FromCharsReverse prefix) (FromChars rest)))
      )

type family Parse' (parsed :: [Char]) (rest :: [Char]) (tags :: [Tag]) :: [Tag] where
  Parse' p '[] ts = ts
  Parse' p ('.' : xs) ts = Parse' ('.' : p) xs ts
  Parse' p ('?' : xs) ts = Parse' ('?' : p) xs (Tag'QuestionMark : ts)
  Parse' p ('>' : xs) ts = Parse' ('>' : p) xs (Tag'RightArrow : ts)
  Parse' p ('<' : xs) ts = Parse' ('<' : p) xs (Tag'LeftArrow : ts)
  Parse' p ('+' : xs) ts = Parse' ('+' : p) xs (Tag'Plus : ts)
  Parse' p ('!' : xs) ts = Parse' ('!' : p) xs (Tag'ExclamationMark : ts)
  Parse' p ('%' : x : xs) ts =
    If
      (CharBetween x '1' '9')
      (Parse' (x : '%' : p) xs (Tag'PercentageNumber (DigitNat x) : ts))
      ( If
          (IsSpecial x)
          (UnexpectedCharacterError x "Expected a letter or a digit\nafter '%'" p (x : xs))
          (Parse' (x : '%' : p) xs (Tag'PercentageName (ConsSymbol x "") : ts))
      )
  Parse' p (x : xs) (Tag'PercentageName s : ts) = Parse' (x : p) xs (Tag'PercentageName (AppendChar s x) : ts)
  Parse' p (x : xs) (Tag'PercentageNumber n : ts) =
    If
      (CharBetween x '0' '9')
      (Parse' (x : p) xs (Tag'PercentageNumber (n TL.* 10 + DigitNat x) : ts))
      (UnexpectedCharacterError x "Expected a digit or a special character\nafter a digit" p (x : xs))
  Parse' p (x : xs) ts =
    If
      (Eval (Or '[CharBetween x '0' '9']))
      (UnexpectedCharacterError x "Expected a letter" p (x : xs))
      (Parse' (x : p) xs (Tag'FieldName (FromChar x) : ts))
  Parse' _ _ _ = TypeError (Text "cornercase!")

type family Parse (a :: Symbol) :: [Tag] where
  Parse a = Eval (Reverse (Parse' '[] (ToChars a) '[]))

class KnownTag (a :: Tag) where
  tagVal :: TagVal

instance KnownTag Tag'QuestionMark where tagVal = TagVal'QuestionMark
instance KnownTag Tag'RightArrow where tagVal = TagVal'RightArrow
instance KnownTag Tag'LeftArrow where tagVal = TagVal'LeftArrow
instance KnownTag Tag'Plus where tagVal = TagVal'Plus
instance KnownTag Tag'ExclamationMark where tagVal = TagVal'ExclamationMark
instance (KnownSymbol a) => KnownTag (Tag'PercentageName a) where tagVal = TagVal'PercentageName (symbolVal (Proxy @a))
instance (KnownNat a) => KnownTag (Tag'PercentageNumber a) where tagVal = TagVal'PercentageNumber (natVal (Proxy @a))
instance (KnownSymbol a) => KnownTag (Tag'FieldName a) where tagVal = TagVal'FieldName (symbolVal (Proxy @a))

class KnownTags (a :: [Tag]) where
  tagVals :: [TagVal]

instance KnownTags '[] where
  tagVals = []

instance (KnownTag x, KnownTags xs) => KnownTags (x : xs) where
  tagVals = (tagVal @x) : tagVals @xs

-- tests

ex1 :: String
ex1 = symbolVal (Proxy @(AppendSymbol "a" "b"))

-- >>> ex1
-- "ab"

-- >>> tagVals @(Parse "a?.!a%33")
-- [TagVal'FieldName "a",TagVal'QuestionMark,TagVal'ExclamationMark,TagVal'FieldName "a",TagVal'PercentageNumber 33]

-- >>> tagVals @(Parse "a?.!a%3a")
-- Unexpected character: a
-- Expected a digit or a special character
-- after a digit
-- in a?.!a%3
-- in a?.!a%3a
-- In the expression: tagVals @(Parse "a?.!a%3a")
-- In an equation for `it_aOV9': it_aOV9 = tagVals @(Parse "a?.!a%3a")
