module Hedgehog.Golden.Internal.Source
  ( -- * Functions for producing diff
    addLineNumber
  , addLineNumbers
  , added
  , boxBottom
  , boxTop
  , removed
  , wrap
  -- * Colors
  , green
  , red
  , white
  , yellow
  ) where

import           Prelude

import           Data.Text (Text)
import qualified Data.Text as Text

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
boxTop = Text.replicate 5 "─" <> "┬" <> Text.replicate 55 "─"

boxBottom :: Text
boxBottom = Text.replicate 5 "─" <> "┴" <> Text.replicate 55 "─"

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
