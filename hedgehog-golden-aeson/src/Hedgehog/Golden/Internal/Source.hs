module Hedgehog.Golden.Internal.Source
  ( addLineNumbers
  , added
  , boxBottom
  , boxTop
  , isInteractive
  , removed
  , renderCallsite
  -- * Colors
  , green
  , red
  , white
  , yellow
  ) where

import           Prelude

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Stack (SrcLoc(..))

isInteractive :: SrcLoc -> Bool
isInteractive SrcLoc{..} =
  srcLocFile == "<interactive>"

renderCallsite :: SrcLoc -> IO [Text]
renderCallsite s@SrcLoc{..} =
  if isInteractive s then pure []
  else do
    srcLines <- Text.lines <$> Text.readFile srcLocFile
    pure
      . addSourceLoc srcLocFile srcLocStartLine
      . wrap boxTop boxBottom
      . take (4 + srcLocEndLine - srcLocStartLine)
      . drop (srcLocStartLine - 2)
      . addUnderline srcLocStartLine srcLocStartCol srcLocEndCol
      . addLineNumbers
      $ srcLines

addSourceLoc :: FilePath -> Int -> [Text] -> [Text]
addSourceLoc (Text.pack -> fp) (Text.pack . show -> lineNumber) txt =
  ["", yellow "New golden file generated from: " <> fp <> ":" <> lineNumber ] ++ txt

addUnderline :: Int -> Int -> Int -> [Text] -> [Text]
addUnderline line startCol endCol txt =
  take (line) txt ++ underline startCol endCol ++ drop line txt

underline :: Int -> Int -> [Text]
underline start end =
  ["     │" <> red (Text.replicate (start - 1) " " <> Text.replicate (end - start) "^")]

addLineNumbers :: [Text] -> [Text]
addLineNumbers =
  fmap (uncurry addLineNumber) . zip [1..]

addLineNumber :: Int -> Text -> Text
addLineNumber lineNumber line
  | lineNumber < 10 = "   " <> Text.pack (show lineNumber) <> " │" <> line
  | lineNumber < 100 = "  " <> Text.pack (show lineNumber) <> " │" <> line
  | otherwise = " " <> Text.pack (show lineNumber) <> " │" <> line

wrap :: Text -> Text -> [Text] -> [Text]
wrap start end mid = [start] ++ mid ++ [end]

boxTop :: Text
boxTop = Text.replicate 5 "─" <> "┬" <> Text.replicate 115 "─"

boxBottom :: Text
boxBottom = Text.replicate 5 "─" <> "┴" <> Text.replicate 115 "─"

red :: Text -> Text
red t = "\ESC[31;1m" <> t <> "\ESC[0m"

yellow :: Text -> Text
yellow t = "\ESC[33;1m" <> t <> "\ESC[0m"

green :: Text -> Text
green t = "\ESC[32;1m" <> t <> "\ESC[0m"

white :: Text -> Text
white t = "\ESC[37;1m" <> t <> "\ESC[0m"

added :: Text -> Text
added = green . ("+" <>)

removed :: Text -> Text
removed  = red . ("-" <>)
