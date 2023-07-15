{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Lens.Barlow where

import Control.Lens (Optic)
import Data.Lens.Barlow.Construction ( ConstructBarlow(..) )
import Data.Lens.Barlow.Parser (Parse)

barlow :: forall path {p} {f} {s} {t} {a} {b}. (ConstructBarlow (Parse path) p f s t a b) => Optic p f s t a b
barlow = constructBarlow @(Parse path)

bw :: forall path {p} {f} {s} {t} {a} {b}. (ConstructBarlow (Parse path) p f s t a b) => Optic p f s t a b
bw = barlow @path