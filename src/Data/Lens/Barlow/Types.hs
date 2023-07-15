module Data.Lens.Barlow.Types where

import GHC.TypeLits (Natural, Symbol)
import GHC.TypeNats (Nat)

data Tag
  = Tag'Dot
  | Tag'QuestionMark
  | Tag'RightArrow
  | Tag'LeftArrow
  | Tag'Plus
  | Tag'ExclamationMark
  | Tag'PercentageName Symbol
  | Tag'PercentageNumber Nat
  | Tag'Name Symbol

data TagVal
  = TagVal'Dot
  | TagVal'QuestionMark
  | TagVal'RightArrow
  | TagVal'LeftArrow
  | TagVal'Plus
  | TagVal'ExclamationMark
  | TagVal'PercentageName String
  | TagVal'PercentageNumber Natural
  | TagVal'Name String
  deriving (Show, Eq)
