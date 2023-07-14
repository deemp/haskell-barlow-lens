{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Lens.Barlow.Construction () where

import Control.Lens (Choice, Optic, Profunctor, traversed, _Just, _Left, _Right)

import Data.Generics.Product.Fields
import Data.Generics.Wrapped (Wrapped (wrappedIso))
import Data.Lens.Barlow.Types
import GHC.Generics (Generic)
import Data.Lens.Barlow.ParserTF(Parse)

data TagInstance
  = Other'QuestionMark1
  | Other'RightArrow1
  | Other'LeftArrow1
  | Other'Plus1
  | Other'ExclamationMark1
  | Other'RecordSelector1
  | Other'QuestionMark2
  | Other'RightArrow2
  | Other'LeftArrow2
  | Other'Plus2
  | Other'ExclamationMark2
  | Other'RecordSelector2

type family SelectTagInstance (lenses :: [Tag]) s t a b :: TagInstance where
  SelectTagInstance (Tag'QuestionMark : '[]) (Maybe a) (Maybe b) a b = Other'QuestionMark1
  SelectTagInstance (Tag'QuestionMark : rest) (Maybe restA) (Maybe restB) a b = Other'QuestionMark2
  SelectTagInstance (Tag'RightArrow : '[]) (Either l a) (Either l b) a b = Other'RightArrow1
  SelectTagInstance (Tag'RightArrow : rest) (Either l restA) (Either l restB) a b = Other'RightArrow2
  SelectTagInstance (Tag'LeftArrow : '[]) (Either a r) (Either b r) a b = Other'LeftArrow1
  SelectTagInstance (Tag'LeftArrow : rest) (Either restA r) (Either restB r) a b = Other'LeftArrow2
  SelectTagInstance (Tag'Plus : '[]) (t a) (t b) a b = Other'Plus1
  SelectTagInstance (Tag'Plus : rest) (t restA) (t restB) a b = Other'Plus2
  SelectTagInstance (Tag'ExclamationMark : '[]) s t a b = Other'ExclamationMark1
  SelectTagInstance (Tag'ExclamationMark : rest) s t a b = Other'ExclamationMark2
  SelectTagInstance (Tag'FieldName sym : '[]) s t a b = Other'RecordSelector1
  SelectTagInstance (Tag'FieldName sym : rest) s t a b = Other'RecordSelector2

class ConstructBarlow1 (tag :: TagInstance) (lenses :: [Tag]) p f s t a b where
  constructBarlow :: Optic p f s t a b

type ConstructBarlow lenses p f s t a b =
  ConstructBarlow1 (SelectTagInstance lenses s t a b) lenses p f s t a b

instance (Choice p, Applicative f) => ConstructBarlow1 Other'QuestionMark1 (Tag'QuestionMark : '[]) p f (Maybe a) (Maybe b) a b where
  constructBarlow = _Just
instance (Choice p, Applicative f) => ConstructBarlow1 Other'RightArrow1 (Tag'RightArrow : '[]) p f (Either l a) (Either l b) a b where
  constructBarlow = _Right
instance (Choice p, Applicative f) => ConstructBarlow1 Other'LeftArrow1 (Tag'LeftArrow : '[]) p f (Either a r) (Either b r) a b where
  constructBarlow = _Left

-- TODO generalize p ~ (->)?
instance (Traversable t, Applicative f, p ~ (->)) => ConstructBarlow1 Other'Plus1 (Tag'Plus : '[]) p f (t a) (t b) a b where
  constructBarlow = traversed
instance (Profunctor p, Wrapped s t a b, Functor f) => ConstructBarlow1 Other'ExclamationMark1 (Tag'ExclamationMark : '[]) p f s t a b where
  constructBarlow = wrappedIso
instance (HasField sym s t a b, Functor f, p ~ (->)) => ConstructBarlow1 Other'RecordSelector1 (Tag'FieldName sym : '[]) p f s t a b where
  constructBarlow = field @sym

instance (Choice p, ConstructBarlow rest p f restA restB a b, Applicative f) => ConstructBarlow1 Other'QuestionMark2 (Tag'QuestionMark : rest) p f (Maybe restA) (Maybe restB) a b where
  constructBarlow = _Just . constructBarlow @(SelectTagInstance rest restA restB a b) @rest
instance (Choice p, ConstructBarlow rest p f restA restB a b, Applicative f) => ConstructBarlow1 Other'RightArrow2 (Tag'RightArrow : rest) p f (Either l restA) (Either l restB) a b where
  constructBarlow = _Right . constructBarlow @(SelectTagInstance rest restA restB a b) @rest
instance (Choice p, ConstructBarlow rest p f restA restB a b, Applicative f) => ConstructBarlow1 Other'LeftArrow2 (Tag'LeftArrow : rest) p f (Either restA r) (Either restB r) a b where
  constructBarlow = _Left . constructBarlow @(SelectTagInstance rest restA restB a b) @rest
instance (p ~ (->), Traversable t, ConstructBarlow rest p f restA restB a b, Applicative f) => ConstructBarlow1 Other'Plus2 (Tag'Plus : rest) p f (t restA) (t restB) a b where
  constructBarlow = traversed . constructBarlow @(SelectTagInstance rest restA restB a b) @rest

instance (Profunctor p, Functor f, ConstructBarlow rest p f restA restB a b, Wrapped s t restA restB) => ConstructBarlow1 Other'ExclamationMark2 (Tag'ExclamationMark : rest) p f s t a b where
  constructBarlow = wrappedIso . constructBarlow @(SelectTagInstance rest restA restB a b) @rest
instance (p ~ (->), Functor f, ConstructBarlow rest p f restA restB a b, HasField sym s t restA restB) => ConstructBarlow1 Other'RecordSelector2 (Tag'FieldName sym : rest) p f s t a b where
  constructBarlow = field @sym . constructBarlow @(SelectTagInstance rest restA restB a b) @rest


barlow :: forall path {p} {f} {s} {t} {a} {b}. ConstructBarlow (Parse path) p f s t a b => Optic p f s t a b
barlow = constructBarlow @(SelectTagInstance (Parse path) s t a b) @(Parse path)

newtype A a = A { a :: a } deriving stock Generic
newtype B = B { b :: Int } deriving stock Generic

-- >>> import Control.Lens
-- >>> A{ a = B{ b = 1 } } ^. barlow @"a.b"
-- >>> A{ a = Just B{ b = 1 } } ^? barlow @"a?.b"
-- >>> A{ a = Nothing :: Maybe B } ^? barlow @"a?.b"
-- 1
-- Just 1
-- Nothing

