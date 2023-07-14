{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Lens.Barlow.Types where

import GHC.TypeLits (Nat, Symbol)

data Tag
  = Tag'QuestionMark
  | Tag'RightArrow
  | Tag'LeftArrow
  | Tag'Plus
  | Tag'ExclamationMark
  | Tag'PercentageName Symbol
  | Tag'PercentageNumber Nat
  | Tag'FieldName Symbol

data TagVal
  = TagVal'QuestionMark
  | TagVal'RightArrow
  | TagVal'LeftArrow
  | TagVal'Plus
  | TagVal'ExclamationMark
  | TagVal'PercentageName String
  | TagVal'PercentageNumber Integer
  | TagVal'FieldName String
  deriving (Show)
