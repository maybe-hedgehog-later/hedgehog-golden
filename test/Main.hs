module Main (main) where

import           Prelude

import           Hedgehog.Golden
import qualified Hedgehog.Golden.Aeson as Aeson
import qualified Hedgehog.Gen as Gen

main :: IO ()
main = goldenTests_ "Simple Tests"
  [ Aeson.goldenTest Gen.hexit
  , Aeson.goldenTest Gen.bool
  ]
