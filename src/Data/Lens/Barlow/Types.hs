{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Lens.Barlow.Types where

import Data.Data (Proxy (..))
import GHC.TypeLits (Symbol)

data LensType where
  QuestionMark :: LensType
  RightArrow :: LensType
  LeftArrow :: LensType
  Plus :: LensType
  ExclamationMark :: LensType
  Percentage :: forall k. k -> LensType
  RecordField :: Symbol -> LensType

p :: Proxy 'QuestionMark
p = Proxy

data TList where
  TNil :: TList
  TCons :: forall k. k -> TList -> TList

data Nat where
  Z :: Nat
  S :: Nat -> Nat

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8