{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Lens.Barlow.Construction where

import Control.Lens (Optic, traversed, _Just, _Left, _Right, (^?))
import Control.Lens.Iso (Profunctor)
import Control.Lens.Prism (Choice)
import Data.Generics.Product.Fields (HasField (field))
import Data.Generics.Product.Positions (HasPosition (position))
import Data.Generics.Sum.Constructors (AsConstructor (_Ctor))
import Data.Generics.Wrapped (Wrapped (wrappedIso))
import Data.Lens.Barlow.Types (Tag (..))

-- Translated https://github.com/sigma-andex/purescript-barlow-lens/blob/main/src/Data/Lens/Barlow/Construction.purs

class ConstructBarlow (path :: [Tag]) p f s t a b | path s -> a, path t -> b where
  constructBarlow :: Optic p f s t a b

instance ConstructBarlow '[] p f s t s t where
  constructBarlow = id

instance (Choice p, ConstructBarlow path p f s t a b, Applicative f) => ConstructBarlow (Tag'RightArrow : path) p f (Either x s) (Either x t) a b where
  constructBarlow = _Right . constructBarlow @path

instance (Choice p, ConstructBarlow path p f s t a b, Applicative f) => ConstructBarlow (Tag'LeftArrow : path) p f (Either s x) (Either t x) a b where
  constructBarlow = _Left . constructBarlow @path

instance (Choice p, ConstructBarlow path p f q w a b, AsConstructor ctor s t q w, Applicative f) => ConstructBarlow (Tag'PercentageName ctor : path) p f s t a b where
  constructBarlow = _Ctor @ctor . constructBarlow @path

instance (p ~ (->), Applicative f, ConstructBarlow path p f s t a b, Traversable tr) => ConstructBarlow (Tag'Plus : path) p f (tr s) (tr t) a b where
  constructBarlow = traversed . constructBarlow @path

instance (Profunctor p, ConstructBarlow path p f q w a b, Functor f, Wrapped s t q w) => ConstructBarlow (Tag'ExclamationMark : path) p f s t a b where
  constructBarlow = wrappedIso . constructBarlow @path

instance (Choice p, ConstructBarlow path p f s t a b, Applicative f) => ConstructBarlow (Tag'QuestionMark : path) p f (Maybe s) (Maybe t) a b where
  constructBarlow = _Just . constructBarlow @path

instance (p ~ (->), Functor f, ConstructBarlow path p f q w a b, HasPosition pos s t q w) => ConstructBarlow (Tag'PercentageNumber pos : path) p f s t a b where
  constructBarlow = position @pos . constructBarlow @path

instance (p ~ (->), Functor f, ConstructBarlow path p f q w a b, HasField sym s t q w) => ConstructBarlow (Tag'Name sym : path) p f s t a b where
  constructBarlow = field @sym . constructBarlow @path