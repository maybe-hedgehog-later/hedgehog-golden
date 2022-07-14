module Hedgehog.Golden.Types where

import           Prelude

import           Data.Text (Text)
import           Hedgehog (Seed, Size)

type ValueGenerator = Size -> Seed -> [Text]

type ValueReader = Text -> Either Text ()

data GoldenTest
  = NewFile FilePath ValueGenerator
  | ExistingFile FilePath ValueGenerator (Maybe ValueReader)
