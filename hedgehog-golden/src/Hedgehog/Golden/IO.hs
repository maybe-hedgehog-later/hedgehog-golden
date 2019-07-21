module Hedgehog.Golden.IO
  ( ComparisonResult(..)
  , compareFileToText
  ) where

import Prelude (($), (.), (>>=), Bool(..), FilePath, IO, pure)

import Control.Monad.Extra (ifM)
import Data.Algorithm.Diff (Diff(..), getDiff)
import Data.List (all)
import Data.Text (Text, lines)
import Data.Text.IO (readFile)
import System.Directory (doesFileExist)

data ComparisonResult
  = NoChange
  | Difference [Diff Text]
  | NonExistingFile

-- | Compares the contents of the given 'fp' to the supplied 'exp' text
compareFileToText :: FilePath -> Text -> IO ComparisonResult
compareFileToText fp exp =
  ifM
    (doesFileExist fp)
    (readFile fp >>= pure . compareText exp)
    (pure NonExistingFile)

compareText :: Text -> Text -> ComparisonResult
compareText (lines -> exp) (lines -> act) =
  let
    hasDifference = all $ \case
      Both _ _ -> True
      First _  -> False
      Second _ -> False
    diff = getDiff exp act
  in
    if hasDifference diff then Difference diff
    else NoChange
