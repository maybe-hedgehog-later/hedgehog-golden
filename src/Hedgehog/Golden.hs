module Hedgehog.Golden
  ( goldenTests
  , goldenTests_
  , GoldenTest
  ) where

import Prelude

import           Control.Monad (unless)
import           Data.Algorithm.Diff (Diff(..), getDiff)
import           Data.Traversable (traverse)
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Stack (CallStack, getCallStack)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Golden.Internal.Source as Source
import           System.Exit (exitFailure)
import           System.Directory (createDirectoryIfMissing)
import           Hedgehog.Golden.Types (GoldenTest(..), GroupName(..))
import           Hedgehog.Golden.Types (TestName(..), ValueGenerator, ValueReader)
import           Hedgehog.Golden.Aeson (decodeSeed)

data TestResult
  = NewFileFailure FilePath
  | ValueReadError Text
  | FileError FilePath Text
  | Success
  deriving Eq

goldenTests_ :: GroupName -> [IO GoldenTest] -> IO ()
goldenTests_ groupName tests = do
  res <- and <$> goldenTests groupName tests
  unless res exitFailure

goldenTests :: GroupName -> [IO GoldenTest] -> IO [Bool]
goldenTests groupName tests = do
  printGroupName groupName
  testResults <- sequence tests >>= traverse applyTest

  let
    errorMessages =
      catMaybes $ getErrorSummary <$> testResults

    printOutput errors =
      if null errors then pure ()
      else Text.putStrLn (Source.white "\n▸ Summary") >> traverse_ printErrorRoot errors

  printOutput errorMessages
  pure $ fmap (== Success) testResults

getErrorSummary :: TestResult -> Maybe Text
getErrorSummary = \case
  Success ->
    Nothing
  NewFileFailure fp ->
    Just $ "Golden file did not exist: " <> Text.pack fp
  FileError fp msg ->
    Just $ "Encountered file error in " <> Source.yellow (Text.pack fp) <> ":\n    " <> msg
  ValueReadError msg ->
    Just $ "Encountered value read error: " <> msg

applyTest :: GoldenTest -> IO TestResult
applyTest = \case
  NewFile name cs fp gen -> printName name >> newGoldenFile cs fp gen
  ExistingFile name cs fp gen reader -> printName name >> existingGoldenFile cs fp gen reader

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
    callsite <- Source.renderCallsite "New golden file generated from" srcLoc

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
      pure $ NewFileFailure fp

handleInputChoice :: FilePath -> [Text] -> IO TestResult
handleInputChoice fp fileLines = getChoice >>= \case
  'a' -> saveNew
  'A' -> saveNew

  'r' -> pure $ NewFileFailure fp
  'R' -> pure $ NewFileFailure fp

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
  , "Accept new golden file?"
  , ""
  , Source.green  "  A" <> Source.white ")ccept" <> "     save new file"
  , Source.red    "  r" <> Source.white ")eject" <> "     keep old golden file"
  , ""
  ]

existingGoldenFile :: CallStack -> FilePath -> ValueGenerator -> Maybe ValueReader -> IO TestResult
existingGoldenFile cs fp gen reader = do
    let srcLoc = snd . head . getCallStack $ cs
    callsite <- Source.renderCallsite "Comparison error source" srcLoc
    getSeedAndLines >>= \case
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
          if hasDifference comparison then do
            printError "Failed in serialization comparison"
            Text.putStrLn . Text.intercalate "\n    " $ callsite
            Text.putStrLn $ Source.yellow "    Difference in: " <> Text.pack fp
            printDifference comparison
            if Source.isInteractive srcLoc then do
              Text.putStrLn . Text.intercalate "\n    " $ renderAcceptNew
              handleInputChoice fp newLines
            else
              pure $ NewFileFailure fp
          else do
            printSuccess "Passed write test"
            runDecodeTest
      Left (Text.pack -> err) ->
        pure (FileError fp err)
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

printError :: Text -> IO ()
printError msg = Text.putStrLn $ Source.red "    ✗ " <> msg

printErrorRoot :: Text -> IO ()
printErrorRoot msg = Text.putStrLn $ Source.red "  ✗ " <> msg

printDifference :: [Diff Text] -> IO ()
printDifference =
  mapM_ Text.putStrLn
    . fmap ("    " <>)
    . Source.wrap Source.boxTop Source.boxBottom
    . addLineNumbers 1
    . renderDiff
  where
    renderDiff :: [Diff Text] -> [Diff Text]
    renderDiff =
      fmap $ \case
        Both text _ -> Both (" " <> text) (" " <> text)
        First text  -> First $ Source.red $ "-" <> text
        Second text -> Second $ Source.green $ "+" <> text

    addLineNumbers :: Int -> [Diff Text] -> [Text]
    addLineNumbers _ [] = []
    addLineNumbers i (d : ds) = case d of
      Both text _ ->
        Source.addLineNumber i text : addLineNumbers (i + 1) ds
      First text ->
        Source.addLineNumber i text : addLineNumbers i ds
      Second text ->
        Source.addLineNumber i text : addLineNumbers (i + 1) ds
