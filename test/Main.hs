{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Prelude

import           Control.Monad (unless)
import           Hedgehog (Property)
import           Hedgehog (checkParallel, discover)
import qualified Hedgehog.Golden.Aeson as Aeson
import qualified Hedgehog.Gen as Gen
import           System.Exit (exitFailure)

prop_hexit :: Property
prop_hexit = Aeson.goldenProperty Gen.hexit

prop_bool :: Property
prop_bool = Aeson.goldenProperty Gen.bool

tests :: IO Bool
tests = checkParallel $$discover

main :: IO ()
main = tests >>= flip unless exitFailure
