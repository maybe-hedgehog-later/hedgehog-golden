module Hedgehog.Golden
  ( goldenTests
  , GoldenTest
  ) where

import Prelude

import           Control.Monad (when)
import           Data.Algorithm.Diff (Diff(..), getDiff)
import           Data.Traversable (traverse)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Stack (CallStack, getCallStack)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Golden.Internal.Source as Source
import           System.Exit (exitFailure)
import           System.Directory (createDirectoryIfMissing)
import           Hedgehog.Golden.Types (GoldenTest(..), GroupName(..))
import           Hedgehog.Golden.Types (TestName(..), ValueGenerator, ValueReader)
import           Hedgehog.Golden.Aeson (decodeSeed)

data TestResult
  = NewFileFailure
  | ValueReadError Text
  | FileError Text
  | ComparisonFailure FilePath [Diff Text]
  | Success
  deriving Eq

goldenTests :: GroupName -> [IO GoldenTest] -> IO ()
goldenTests groupName tests = do
  printGroupName groupName
  sequence tests >>= traverse applyTest >>= checkErrors

checkErrors :: [TestResult] -> IO ()
checkErrors results =
  when (any (/= Success) results) exitFailure

applyTest :: GoldenTest -> IO TestResult
applyTest = \case
  NewFile name cs fp gen -> printName name >> newGoldenFile cs fp gen
  ExistingFile name _ fp gen reader -> printName name >> existingGoldenFile fp gen reader

newGoldenFile :: CallStack -> FilePath -> ValueGenerator -> IO TestResult
newGoldenFile cs fp gen =
  let
    outputLines = Text.putStrLn . Text.intercalate "\n    "
    srcLoc = snd . head . getCallStack $ cs
    renderAddedFile newLines =
      [ Source.yellow "Generated golden will be saved in: " <> Text.pack fp] ++
      [ Source.boxTop ] ++
      Source.addLineNumbers (Source.added <$> newLines) ++
      [ Source.boxBottom ]
  in do
    seed <- Seed.random
    callsite <- Source.renderCallsite srcLoc

    -- Lines for new file golden file:
    let newLines = gen seed

    -- Run interactive mode if run via repl:
    if Source.isInteractive srcLoc then do
      outputLines $ callsite ++ renderAddedFile newLines
      outputLines renderAcceptNew
      handleInputChoice fp newLines
    else do
      outputLines newFileError
      outputLines $ callsite ++ renderAddedFile newLines
      pure NewFileFailure

handleInputChoice :: FilePath -> [Text] -> IO TestResult
handleInputChoice fp fileLines = getChoice >>= \case
  'a' -> saveNew
  'A' -> saveNew

  'r' -> pure NewFileFailure
  'R' -> pure NewFileFailure

  _   -> handleInputChoice fp fileLines
  where
    getChoice = getChar <* clearLine
    clearLine = putChar '\r' >> putChar ' ' >> putChar '\r'

    fileOutput = Text.intercalate "\n" fileLines

    saveNew = do
      createDirectoryIfMissing False "golden"
      Text.writeFile fp fileOutput
      Text.putStrLn "    Created new file!"
      pure Success

newFileError :: [Text]
newFileError =
  [ Source.red "    ✗ " <> "New file, re-run tests interactively to add missing file"
  ]

renderAcceptNew :: [Text]
renderAcceptNew =
  [ ""
  , "  Accept new golden file?"
  , ""
  , Source.green  "    A" <> Source.white ")ccept" <> "     save new file"
  , Source.red    "    r" <> Source.white ")eject" <> "     keep old golden file"
  , ""
  ]

existingGoldenFile :: FilePath -> ValueGenerator -> Maybe ValueReader -> IO TestResult
existingGoldenFile fp gen reader = getSeedAndLines >>= \case
  Right (seed, existingLines) ->
    let
      newLines = gen seed
      comparison = getDiff existingLines newLines
      hasDifference = any $ \case
        Both _ _ -> False
        First _  -> True
        Second _ -> True
      runDecodeTest = case reader of
        Just r ->
          either (pure . ValueReadError) (const $ printSuccess "Passed read test" >> pure Success) $
            r . Text.intercalate "\n" $ existingLines
        Nothing -> pure Success
    in
      if hasDifference comparison then
        pure (ComparisonFailure fp comparison)
      else do
        printSuccess "Passed write test"
        runDecodeTest
  Left (Text.pack -> err) ->
    pure (FileError err)
  where
    getSeedAndLines = do
      fileContents <- Text.readFile fp
      pure . fmap (, Text.lines fileContents) $ decodeSeed fileContents


-- Print functions:
printGroupName :: GroupName -> IO ()
printGroupName (GroupName name) = Text.putStrLn . Source.white $ "\n━━━ " <> name <> " ━━━"

printName :: TestName -> IO ()
printName (TestName name) = Text.putStrLn $ "  · " <> Source.white name

printSuccess :: Text -> IO ()
printSuccess msg = Text.putStrLn $ Source.green "    ✓ " <> msg
