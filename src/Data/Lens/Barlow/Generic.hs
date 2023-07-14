{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Lens.Barlow.Generic where

import Control.Lens
import GHC.Generics
import Data.Profunctor
import Data.Kind

-- import Data.Profunctor (Strong)

_SumRight :: forall l r a. Prism' ((l :+: r) a) (r a)
_SumRight = prism R1 \case
  R1 r -> Right r
  anotherCase -> Left anotherCase

_SumLeft :: forall l r a. Prism' ((l :+: r) a) (l a)
_SumLeft = prism L1 \case
  L1 l -> Right l
  anotherCase -> Left anotherCase

_ProductLeft :: forall l r a. Lens' ((l :*: r) a) (l a)
_ProductLeft = lens (\(first :*: _) -> first) (\(_ :*: second) x -> x :*: second)

_ProductRight :: forall l r a. Lens' ((l :*: r) a) (r a)
_ProductRight = lens (\(_ :*: second) -> second) (\(first :*: _) x -> first :*: x)

_Constructor :: forall {k} (a :: k -> Type) sym x m2 m3. Iso' (C1 (MetaCons sym m2 m3) a x) (a x)
_Constructor = iso unwrapC wrapC
 where
  unwrapC (M1 l) = l
  wrapC = M1

_Argument :: forall a s1 s3. Iso' (S1 s1 (Rec0 a) s3) a
_Argument = iso unwrapA wrapA
 where
  unwrapA (M1 (K1 a)) = a
  wrapA = M1 . K1

_ToGeneric :: forall input output. (Generic input) => Iso' input (Rep input output)
_ToGeneric = iso GHC.Generics.from GHC.Generics.to

-- TODO fix
_NoArguments :: forall a p. Strong p => p () () -> p a a
_NoArguments = undefined
-- _NoArguments = lens (Const ()) Const
