module Main (main) where

import           Prelude

import           Hedgehog.Golden
import qualified Hedgehog.Gen as Gen

main :: IO ()
main = goldenTests
  [ aesonDiff Gen.hexit
  ]
