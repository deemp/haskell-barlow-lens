{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Data.Lens.Barlow.Construction () where

import Control.Lens (Choice, Optic, Profunctor)
import Data.Data (Proxy (..))
import Data.Lens.Barlow.Generic
import Data.Lens.Barlow.Types
import Data.Profunctor (Strong)
import Data.Profunctor.Traversing
import Data.Generics.Product.Fields
import GHC.Generics
import GHC.TypeLits

data TagPercentage
  = PercentageSingleton'1
  | PercentageSingleton'2
  | PercentageSingleton'3
  | PercentageSingleton'4
  | PercentageLeftSum'1
  | PercentageLeftSum'2
  | PercentageLeftSum'3
  | PercentageLeftSum'4
  | PercentageRightSum'1
  | PercentageRightSum'2
  | PercentageRightSum'3
  | PercentageRightSum'4
  | PercentageLeftProduct'1
  | PercentageLeftProduct'2

type family SelectTag' (lenses :: [Tag]) s a :: TagPercentage where
  -- percentage singleton
  SelectTag' (Tag'PercentageName sym : '[]) (C1 (MetaCons sym _ _) V1 _) () = PercentageSingleton'1
  SelectTag' (Tag'PercentageName sym : '[]) (C1 (MetaCons sym _ _) (S1 _ (Rec0 r)) _) r = PercentageSingleton'2
  SelectTag' (Tag'PercentageName sym : _) (C1 (MetaCons sym _ _) (S1 _ (Rec0 _)) _) _ = PercentageSingleton'3
  SelectTag' (Tag'PercentageName sym : _) (C1 (MetaCons sym _ _) _ _) _ = PercentageSingleton'4
  -- percentage left sum
  SelectTag' (Tag'PercentageName sym : '[]) ((C1 (MetaCons sym _ _) V1 :+: _) _) () = PercentageLeftSum'1
  SelectTag' (Tag'PercentageName sym : '[]) ((C1 (MetaCons sym _ _) (S1 _ (Rec0 r)) :+: _) _) r = PercentageLeftSum'2
  SelectTag' (Tag'PercentageName sym : _) ((C1 (MetaCons sym _ _) (S1 _ (Rec0 _)) :+: _) _) _ = PercentageLeftSum'3
  SelectTag' (Tag'PercentageName sym : _) ((C1 (MetaCons sym _ _) _ :+: _) _) _ = PercentageLeftSum'4
  -- percentage right sum
  SelectTag' (Tag'PercentageName sym : '[]) ((_ :+: C1 (MetaCons sym _ _) V1) _) () = PercentageRightSum'1
  SelectTag' (Tag'PercentageName sym : '[]) ((_ :+: C1 (MetaCons sym _ _) (S1 _ (Rec0 r))) _) r = PercentageRightSum'2
  SelectTag' (Tag'PercentageName sym : _) ((_ :+: C1 (MetaCons sym _ _) (S1 _ (Rec0 _))) _) _ = PercentageRightSum'3
  SelectTag' (Tag'PercentageName sym : _) ((_ :+: _) _) _ = PercentageRightSum'4
  -- percentage left product
  SelectTag' (Tag'PercentageNumber 1 : '[]) ((S1 _ (Rec0 r) :*: _) _) r = PercentageLeftProduct'1
  SelectTag' (Tag'PercentageNumber 1 : _) ((S1 _ (Rec0 _) :*: _) _) _ = PercentageLeftProduct'2
  -- percentage right product
  SelectTag' (Tag'PercentageNumber 1 : '[]) (S1 _ (Rec0 r) _) r = PercentageLeftProduct'1
  SelectTag' (Tag'PercentageNumber 1 : _) (S1 _ (Rec0 _) _) _ = PercentageLeftProduct'2
  SelectTag' (Tag'PercentageNumber _ : _) ((_ :*: _) _) _ = PercentageLeftProduct'2

-- TODO what if generic representation unbalanced? How to number product terms?

type family SelectTag (lenses :: [Tag]) s t a b :: TagPercentage where
  SelectTag lenses s s a a = SelectTag' lenses s a
  SelectTag _ _ _ _ _ = TypeError (Text "Only monomorphic lenses are supported for records.")

class ConstructBarlowGeneric1 (tag :: TagPercentage) (lenses :: [Tag]) p f s a | lenses s -> a where
  constructBarlowGeneric :: Proxy lenses -> Optic p f s t a b

type ConstructBarlowGeneric lenses p f s t a b =
  ConstructBarlowGeneric1 (SelectTag lenses s t a b) lenses p f s a

--------- PERCENTAGE SINGLETON CONSTRUCTOR -----------
instance (Strong p) => ConstructBarlowGeneric1 PercentageSingleton'1 (Tag'PercentageName sym : '[]) p f (C1 (MetaCons sym m2 m3) V1 c2) () where
  constructBarlowGeneric _ = _Constructor . _NoArguments
instance (Profunctor p) => ConstructBarlowGeneric1 PercentageSingleton'2 (Tag'PercentageName sym : '[]) p f (C1 (MetaCons sym m2 m3) (S1 s1 (Rec0 r)) c3) r where
  constructBarlowGeneric _ = _Constructor . _Argument
instance (Profunctor p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageSingleton'3 (Tag'PercentageName sym : rest) p f (C1 (MetaCons sym m2 m3) (S1 s1 (Rec0 restA)) c3) a where
  constructBarlowGeneric _ = _Constructor . _Argument . constructBarlowGeneric (Proxy :: Proxy rest)
instance (Profunctor p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageSingleton'4 (Tag'PercentageName sym : rest) p f (C1 (MetaCons sym m2 m3) restA c3) a where
  constructBarlowGeneric _ = _Constructor . constructBarlowGeneric (Proxy :: Proxy rest)

--------- PERCENTAGE LEFT SUM -----------------------
instance (Choice p, Strong p) => ConstructBarlowGeneric1 PercentageLeftSum'1 (Tag'PercentageName sym : '[]) p f ((C1 (MetaCons sym m2 m3) V1 :+: s2) s3) () where
  constructBarlowGeneric _ = _SumLeft . _Constructor . _NoArguments
instance (Choice p) => ConstructBarlowGeneric1 PercentageLeftSum'2 (Tag'PercentageName sym : '[]) p f ((C1 (MetaCons sym m2 m3) (S1 s1 (Rec0 r)) :+: s2) s3) r where
  constructBarlowGeneric _ = _SumLeft . _Constructor . _Argument
instance (Choice p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageLeftSum'3 (Tag'PercentageName sym : rest) p f ((C1 (MetaCons sym m2 m3) (S1 s1 (Rec0 restA)) :+: s2) s3) a where
  constructBarlowGeneric _ = _SumLeft . _Constructor . _Argument . constructBarlowGeneric (Proxy :: Proxy rest)
instance (Choice p, ConstructBarlow rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageLeftSum'4 (Tag'PercentageName sym : rest) p f ((C1 (MetaCons sym m2 m3) restA :+: s2) s3) a where
  constructBarlowGeneric _ = _SumLeft . _Constructor . constructBarlowGeneric (Proxy :: Proxy rest)

--------- PERCENTAGE RIGHT SUM -----------------------
instance (Choice p, Strong p) => ConstructBarlowGeneric1 PercentageRightSum'1 (Tag'PercentageName sym : '[]) p f ((s1 :+: C1 (MetaCons sym m2 m3) V1) s3) () where
  constructBarlowGeneric _ = _SumLeft . _Constructor . constructBarlowGeneric (Proxy :: Proxy rest)
instance (Choice p) => ConstructBarlowGeneric1 PercentageRightSum'2 (Tag'PercentageName sym : '[]) p f ((s1 :+: C1 (MetaCons sym m2 m3) (S1 s1_ (Rec0 r))) s3) r where
  constructBarlowGeneric _ = _SumRight . _Constructor . _Argument
instance (Choice p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageRightSum'3 (Tag'PercentageName sym : rest) p f ((s1 :+: C1 (MetaCons sym m2 m3) (S1 s1_ (Rec0 restA))) s3) a where
  constructBarlowGeneric _ = _SumRight . _Constructor . _Argument . constructBarlowGeneric (Proxy :: Proxy rest)
instance (Choice p, ConstructBarlow rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageRightSum'4 (Tag'PercentageName sym : rest) p f ((s1 :+: C1 m restA) s3) a where
  constructBarlowGeneric _ = _SumRight . constructBarlowGeneric (Proxy :: Proxy (Tag'PercentageName sym : rest))

-------- PERCENTAGE LEFT PRODUCT -----------------------
instance (Strong p) => ConstructBarlowGeneric1 PercentageLeftProduct'1 (Tag'PercentageNumber 1 : '[]) p f ((S1 s1 (Rec0 r) :*: s2) s3) r where
  constructBarlowGeneric _ = _ProductLeft . _Argument
instance (Strong p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageLeftProduct'2 (Tag'PercentageNumber 1 : rest) p f ((S1 s1 (Rec0 restA) :*: s2) s3) restA where
  constructBarlowGeneric = _ProductLeft . _Argument . constructBarlowGeneric (Proxy :: Proxy rest)

--------- PERCENTAGE RIGHT PRODUCT -----------------------
instance (Profunctor p) => ConstructBarlowGeneric1 PercentageLeftProduct'1 (Tag'PercentageNumber 1 : '[]) p f (S1 s1 (Rec0 r) s3) r where
  constructBarlowGeneric _ = _Argument
instance (Profunctor p, ConstructBarlowGeneric rest p f restA restA a a) => ConstructBarlowGeneric1 PercentageLeftProduct'2 (Tag'PercentageNumber 1 : rest) p f (S1 s1 (Rec0 restA) s3) a where
  constructBarlowGeneric = _Argument . constructBarlowGeneric (Proxy :: Proxy rest)
instance (Strong p, ConstructBarlowGeneric (Tag'PercentageNumber n : rest) p f restA restA a a, m ~ n + 1) => ConstructBarlowGeneric1 PercentageLeftProduct'2 (Tag'PercentageNumber m : rest) p f ((p1 :*: Rec0 restA) p3) a where
  constructBarlowGeneric _ = _ProductRight . constructBarlowGeneric (Proxy :: Proxy (Tag'PercentageNumber n) rest)

data TagOther
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
  | Other'Generic

type family SelectTagOther (lenses :: [Tag]) s t a b :: TagOther where
  SelectTagOther (Tag'QuestionMark : '[]) (Maybe a) (Maybe b) a b = Other'QuestionMark1
  SelectTagOther (Tag'QuestionMark : rest) (Maybe restA) (Maybe restB) a b = Other'QuestionMark2
  SelectTagOther (Tag'RightArrow : '[]) (Either l a) (Either l b) a b = Other'RightArrow1
  SelectTagOther (Tag'RightArrow : rest) (Either l restA) (Either l restB) a b = Other'RightArrow2
  SelectTagOther (Tag'LeftArrow : '[]) (Either a r) (Either b r) a b = Other'LeftArrow1
  SelectTagOther (Tag'LeftArrow : rest) (Either restA r) (Either restB r) a b = Other'LeftArrow2
  SelectTagOther (Tag'Plus : '[]) (t a) (t b) a b = Other'Plus1
  SelectTagOther (Tag'Plus : rest) (t restA) (t restB) a b = Other'Plus2
  SelectTagOther (Tag'ExclamationMark : '[]) nt nt a a = Other'ExclamationMark1
  SelectTagOther (Tag'ExclamationMark : rest) nt nt a a = Other'ExclamationMark2
  -- TODO https://github.com/sigma-andex/purescript-barlow-lens/blob/295c4b32fbeca052ebfd3665a9071012e654b9c0/src/Data/Lens/Barlow/Construction.purs#L287
  SelectTagOther (Tag'FieldName sym : '[]) (x a) (x b) a b = Other'RecordSelector1
  SelectTagOther (Tag'FieldName sym : rest) (x a) (x b) a b = Other'RecordSelector2
  SelectTagOther tlist s s a a = Other'Generic

class ConstructBarlow1 (tag :: TagOther) (lenses :: [Tag]) p f s t a b | lenses s -> t a b where
  constructBarlow :: Proxy lenses -> Optic p f s t a b

instance (Choice p) => ConstructBarlow1 Other'QuestionMark1 (Tag'QuestionMark : '[]) p f (Maybe a) (Maybe b) a b where
  constructBarlow _ = _Just
instance (Choice p) => ConstructBarlow1 Other'RightArrow1 (Tag'RightArrow : '[]) p f (Either l a) (Either l b) a b where
  constructBarlow _ = _Right
instance (Choice p) => ConstructBarlow1 Other'LeftArrow1 (Tag'LeftArrow : '[]) p f (Either a r) (Either b r) a b where
  constructBarlow _ = _Left
instance (Traversing p, Traversable t) => ConstructBarlow1 Other'Plus1 (Tag'Plus : '[]) p f (t a) (t b) a b where
  constructBarlow _ = traversed
instance (Profunctor p, Rep nt ~ D1 (MetaData m1 m2 m3 True) (C1 c1 (S1 s1 (Rec0 a)))) => ConstructBarlow1 Other'ExclamationMark1 (Tag'ExclamationMark : '[]) p f nt nt a a where
  constructBarlow _ = _Newtype

-- TODO https://github.com/sigma-andex/purescript-barlow-lens/blob/295c4b32fbeca052ebfd3665a9071012e654b9c0/src/Data/Lens/Barlow/Construction.purs#L287
-- should be monomorphic?
instance (Strong p, HasField sym x y a b) => ConstructBarlow1 Other'RecordSelector1 (Tag'FieldName sym : '[]) p f x y a b where
  constructBarlow _ = field @sym
instance (Choice p, ConstructBarlow rest p f restA restB a b) => ConstructBarlow1 Other'QuestionMark2 (Tag'QuestionMark : rest) p f (Maybe restA) (Maybe restB) a b where
  constructBarlow _ = _Just . constructBarlow (Proxy :: Proxy rest)
instance (Choice p, ConstructBarlow rest p f restA restB a b) => ConstructBarlow1 Other'RightArrow2 (Tag'RightArrow : rest) p f (Either l restA) (Either l restB) a b where
  constructBarlow _ = _Right . constructBarlow (Proxy :: Proxy rest)
instance (Choice p, ConstructBarlow rest p f restA restB a b) => ConstructBarlow1 Other'LeftArrow2 (Tag'LeftArrow : rest) p f (Either restA r) (Either restB r) a b where
  constructBarlow _ = _Left . constructBarlow (Proxy :: Proxy rest)
instance (Traversing p, Traversable t, ConstructBarlow rest p f restA restB a b) => ConstructBarlow1 Other'Plus2 (Tag'Plus : rest) p f (t restA) (t restB) a b where
  constructBarlow _ = traversed . constructBarlow (Proxy :: Proxy rest)
instance (ConstructBarlow rest p f restA restA a a, Rep nt ~ D1 (MetaData m1 m2 m3 True) (C1 c1 (S1 s1 (Rec0 restA)))) => ConstructBarlow1 Other'ExclamationMark2 (Tag'ExclamationMark : rest) p f nt nt a a where
  constructBarlow _ = _Newtype . constructBarlow (Proxy :: Proxy rest)

-- TODO https://github.com/sigma-andex/purescript-barlow-lens/blob/295c4b32fbeca052ebfd3665a9071012e654b9c0/src/Data/Lens/Barlow/Construction.purs#L363
-- should be monomorphic?
instance (Strong p, ConstructBarlow rest p f x y a b, HasField sym x y a b) => ConstructBarlow1 Other'RecordSelector2 (Tag'FieldName sym : rest) p f x y a b where
  constructBarlow _ = field @sym . constructBarlow (Proxy :: Proxy rest)

-- TODO https://github.com/sigma-andex/purescript-barlow-lens/blob/295c4b32fbeca052ebfd3665a9071012e654b9c0/src/Data/Lens/Barlow/Construction.purs#L373
instance (Profunctor p, Rep s y ~ repS, ConstructBarlowGeneric tlist p f repS repS a a) => ConstructBarlow1 Other'Generic tlist p f s s a a where
  constructBarlow _ = _ToGeneric . constructBarlowGeneric (Proxy :: Proxy tlist)

type ConstructBarlow lenses p f s t a b =
  ConstructBarlow1 (SelectTagOther lenses s t a b) lenses p f s t a b