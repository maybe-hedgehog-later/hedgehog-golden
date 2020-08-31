{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Prelude

import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON)
import           Hedgehog (Gen, Property)
import           Hedgehog (checkParallel, discover)
import           Hedgehog.Main (defaultMain)
import           Hedgehog.Range (singleton)
import qualified Hedgehog.Golden.Aeson as Aeson
import qualified Hedgehog.Gen as Gen

data HigherKinded a = HigherKinded { wrapped :: a }
  deriving stock (Generic)
  deriving anyclass FromJSON
  deriving anyclass ToJSON

higherKinded :: Gen (HigherKinded Int)
higherKinded = fmap HigherKinded (Gen.int $ singleton 7)

prop_hexit :: Property
prop_hexit = Aeson.goldenProperty Gen.hexit

prop_bool :: Property
prop_bool = Aeson.goldenProperty Gen.bool

prop_higher_kinded :: Property
prop_higher_kinded = Aeson.goldenProperty higherKinded

tests :: IO Bool
tests = checkParallel $$discover

main :: IO ()
main = defaultMain [ tests ]
