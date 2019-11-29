{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Prelude

import           Hedgehog (Property)
import           Hedgehog (checkParallel, discover)
import           Hedgehog.Main (defaultMain)
import qualified Hedgehog.Golden.Aeson as Aeson
import qualified Hedgehog.Gen as Gen

prop_hexit :: Property
prop_hexit = Aeson.goldenProperty Gen.hexit

prop_bool :: Property
prop_bool = Aeson.goldenProperty Gen.bool

tests :: IO Bool
tests = checkParallel $$discover

main :: IO ()
main = defaultMain [ tests ]
