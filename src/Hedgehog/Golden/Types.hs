module Hedgehog.Golden.Types where

import           Prelude

import           Data.Text (Text)
import           Hedgehog (Seed)

type ValueGenerator = Seed -> [Text]

type ValueReader = Text -> Either Text ()

data GoldenTest
  = NewFile FilePath ValueGenerator
  | ExistingFile FilePath ValueGenerator (Maybe ValueReader)
