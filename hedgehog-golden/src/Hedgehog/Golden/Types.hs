module Hedgehog.Golden.Types where

import           Prelude

import           Data.Text (Text)
import           GHC.Stack (CallStack)
import           Hedgehog (Seed)

type ValueGenerator = Seed -> [Text]

data GoldenTest
  = NewFile CallStack FilePath ValueGenerator
  | ExistingFile CallStack FilePath ValueGenerator

