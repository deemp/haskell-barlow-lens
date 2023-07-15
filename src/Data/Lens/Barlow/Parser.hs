{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Lens.Barlow.Parser where

-- Translated https://github.com/sigma-andex/purescript-barlow-lens/blob/main/src/Data/Lens/Barlow/Parser.purs

import Data.Lens.Barlow.Types
import Fcf (Eval, Exp, If, TyEq)
import Fcf.Class.Foldable (Or)
import Fcf.Data.List as DL (Elem, Filter, Reverse)
import GHC.TypeLits

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

type family Parse1 (parsed :: [Char]) (rest :: [Char]) (tags :: [Tag]) :: [Tag] where
  Parse1 p '[] ts = ts
  Parse1 p ('.' : xs) ts = Parse1 ('.' : p) xs (Tag'Dot : ts)
  Parse1 p ('?' : xs) ts = Parse1 ('?' : p) xs (Tag'QuestionMark : ts)
  Parse1 p ('>' : xs) ts = Parse1 ('>' : p) xs (Tag'RightArrow : ts)
  Parse1 p ('<' : xs) ts = Parse1 ('<' : p) xs (Tag'LeftArrow : ts)
  Parse1 p ('+' : xs) ts = Parse1 ('+' : p) xs (Tag'Plus : ts)
  Parse1 p ('!' : xs) ts = Parse1 ('!' : p) xs (Tag'ExclamationMark : ts)
  Parse1 p ('%' : x : xs) ts =
    If
      (CharBetween x '1' '9')
      (Parse1 (x : '%' : p) xs (Tag'PercentageNumber (DigitNat x) : ts))
      ( If
          (IsSpecial x)
          (UnexpectedCharacterError x "Expected a letter or a digit\nafter '%'" p (x : xs))
          (Parse1 (x : '%' : p) xs (Tag'PercentageName (ConsSymbol x "") : ts))
      )
  -- TODO check x is upper when an appropriate function is available
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/19634
  -- https://github.com/ghc-proposals/ghc-proposals/pull/509
  Parse1 p (x : xs) (Tag'PercentageName s : ts) = Parse1 (x : p) xs (Tag'PercentageName (AppendChar s x) : ts)
  Parse1 p (x : xs) (Tag'PercentageNumber n : ts) =
    If
      (CharBetween x '0' '9')
      (Parse1 (x : p) xs (Tag'PercentageNumber (n * 10 + DigitNat x) : ts))
      (UnexpectedCharacterError x "Expected a digit or a special character\nafter a digit" p (x : xs))
  Parse1 p (x : xs) (Tag'Name n : ts) = Parse1 (x : p) xs (Tag'Name (AppendChar n x) : ts)
  Parse1 p (x : xs) ts =
    If
      (Eval (Or '[CharBetween x '0' '9']))
      (UnexpectedCharacterError x "Expected a letter" p (x : xs))
      (Parse1 (x : p) xs (Tag'Name (FromChar x) : ts))
  Parse1 _ _ _ = TypeError (Text "cornercase!")

data DotFilter :: Tag -> Exp Bool

type instance Eval (DotFilter x) = If (Eval (TyEq x Tag'Dot)) False True

type family Parse (a :: Symbol) :: [Tag] where
  Parse a = Eval (Filter DotFilter (Eval (Reverse (Parse1 '[] (ToChars a) '[]))))