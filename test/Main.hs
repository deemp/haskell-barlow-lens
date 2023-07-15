{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens ((^?))
import Data.Data (Proxy (..))
import Data.Lens.Barlow (bw)
import Data.Lens.Barlow.Classes
import Data.Lens.Barlow.Types (TagVal (..))
import GHC.Generics (Generic)
import GHC.TypeLits (symbolVal)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

type T1 = "h1d.%Gb3?>..<+!%B3..%10"

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase (symbolVal (Proxy @T1)) $
        symbolTagVals @T1
          @?= [ TagVal'Name "h1d"
              , TagVal'PercentageName "Gb3"
              , TagVal'QuestionMark
              , TagVal'RightArrow
              , TagVal'LeftArrow
              , TagVal'Plus
              , TagVal'ExclamationMark
              , TagVal'PercentageName "B3"
              , TagVal'PercentageNumber 10
              ]
    , testCase
        ("get" <> symbolVal (Proxy @T1))
        $ (ex8 ^? bw @T1) @?= Just 10
    ]

newtype H = H {h1d :: G} deriving (Generic)
data G = Gb3 (Maybe F) | GaA4 deriving (Generic)
type F = Either C E
type E = Either D String
type D = [C]
newtype C = C B deriving (Generic)
data B = B3 A | B2 deriving (Generic)
data A = A Int Int Int Int Int Int Int Int Int Int deriving (Generic)

ex1 = A 1 2 3 4 5 6 7 8 9 10
ex2 = B3 ex1
ex3 = C ex2
ex4 = [ex3, ex3]
ex5 = Left ex4
ex6 = Right ex5
ex7 = Gb3 (Just ex6)
ex8 = H ex7
