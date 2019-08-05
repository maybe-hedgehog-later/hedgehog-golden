module Hedgehog.Golden.Types where

import           Prelude

import           Data.Text (Text)
import           Data.String (IsString)
import           GHC.Stack (CallStack)
import           Hedgehog (Seed)

type ValueGenerator = Seed -> [Text]

type ValueReader = Text -> Either Text ()

newtype GroupName = GroupName { unGroupName :: Text }
  deriving (Eq, IsString)

newtype TestName = TestName { unTestName :: Text }
  deriving (Eq, IsString)

data GoldenTest
  = NewFile TestName CallStack FilePath ValueGenerator
  | ExistingFile TestName CallStack FilePath ValueGenerator (Maybe ValueReader)
