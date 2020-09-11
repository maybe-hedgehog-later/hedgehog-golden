-- | This module can be used in order to create golden tests for Aeson
--   serializers and deserializers
--
--   @
--   {-\# LANGUAGE TemplateHaskell \#-}
--
--   import           Hedgehog
--   import qualified Hedgehog.Gen as Gen
--   import qualified Hedgehog.Golden.Aeson as Aeson
--
--   -- | A golden test for characters in the hex range
--   prop_char_golden :: Property
--   prop_char_golden = Aeson.goldenProperty Gen.hexit
--
--   tests :: IO Bool
--   tests = checkParallel $$discover
--   @
module Hedgehog.Golden.Aeson
  ( -- * Golden tests for generators
    goldenProperty
  , goldenProperty'
  ) where

import           Prelude

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Algorithm.Diff (PolyDiff(..), Diff, getDiff)
import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as Aeson (eitherDecodeStrict)
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text (intercalate, lines, pack, replace, unpack)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text (readFile, writeFile)
import           Data.Typeable (Typeable, typeRep)
import           Hedgehog (Gen, Property, PropertyT, Seed(..))
import           Hedgehog (success)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Source
import           Hedgehog.Internal.Property (Log(..), Property(..), PropertyConfig(..))
import           Hedgehog.Internal.Property (TerminationCriteria(..))
import           Hedgehog.Internal.Property (defaultConfig, evalM, failWith, writeLog)
import           Hedgehog.Golden.Sample (genSamples)
import           Hedgehog.Golden.Types (GoldenTest(..), ValueGenerator, ValueReader)
import qualified Hedgehog.Golden.Internal.Source as Source
import           System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)

-- | Run a golden test on the given generator
--
--   This will create a file in @golden/<TypeName>.json.new@ in case it does not
--   exist. If it does exist - the golden tests will be run against it
--
goldenProperty :: forall a
   . HasCallStack
  => Typeable a
  => FromJSON a
  => ToJSON a
  => Gen a -> Property
goldenProperty = withFrozenCallStack $ goldenProperty' "golden/"

-- | Same as 'goldenProperty' but allows specifying the directory
--
goldenProperty' :: forall a
   . HasCallStack
  => Typeable a
  => FromJSON a
  => ToJSON a
  => FilePath -> Gen a -> Property
goldenProperty' baseDir gen = withFrozenCallStack $
  Property config . evalM $
    goldenTest baseDir gen >>= \case
      NewFile fileName valGen -> do
        newGoldenFile baseDir fileName valGen
      ExistingFile fileName valGen readerM ->
        existingGoldenFile baseDir fileName valGen readerM
  where
    config = defaultConfig
      { propertyTerminationCriteria = NoConfidenceTermination 1
      , propertyShrinkLimit         = 0
      }

newGoldenFile :: HasCallStack => FilePath -> FilePath -> ValueGenerator -> PropertyT IO ()
newGoldenFile basePath fileName gen = do
  seed <- Seed.random
  -- Create new file
  liftIO $ do
    createDirectoryIfMissing True basePath
    Text.writeFile (fileName <> ".new") . Text.intercalate "\n" . gen $ seed

  -- Annotate output
  currentDir <- liftIO $ getCurrentDirectory
  writeLog . Footnote $ "New golden file generated in: " <> currentDir <> "/" <> fileName <> ".new"
  failWith Nothing "No previous golden file exists"

existingGoldenFile ::
     HasCallStack
  => FilePath -> FilePath -> ValueGenerator -> Maybe ValueReader -> PropertyT IO ()
existingGoldenFile basePath fp gen reader = getSeedAndLines >>= \case
  Right (seed, existingLines) ->
    let
      comparison =
        getDiff existingLines $ gen seed

      hasDifference = any $ \case
        Both _ _ -> False
        First _  -> True
        Second _ -> True

      runDecodeTest = forM_ reader $ \r ->
        either
          (failWith Nothing . (<>) "Failed to deserialize with error: " . Text.unpack)
          (const success)
          (r . Text.intercalate "\n" $ existingLines)
    in
      if hasDifference comparison then do
        liftIO $ do
          createDirectoryIfMissing False basePath
          Text.writeFile (fp <> ".gen") . Text.intercalate "\n" . gen $ seed

        writeLog . Footnote $
          "Different file generated as: " <> fp <> ".gen"

        failWith Nothing . Text.unpack . Text.intercalate "\n" $
          [ "Failed in serialization comparison"
          , ""
          , Source.yellow "Difference when generating: " <> Text.pack fp
          , printDifference comparison
          ]
      else
        runDecodeTest
  Left err ->
    failWith Nothing $ "Couldn't read previous golden file (" <> fp <> ") because: " <> err
  where
    getSeedAndLines = liftIO $ do
      fileContents <- Text.readFile fp
      pure . fmap (, Text.lines fileContents) . decodeSeed $ fileContents

printDifference :: [Diff Text] -> Text
printDifference
  = Text.intercalate "\n"
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

goldenTest :: forall a m
   . Typeable a
  => FromJSON a
  => ToJSON a
  => MonadIO m
  => FilePath -> Gen a -> m GoldenTest
goldenTest prefix gen = do
  let
    typeName = Text.replace " " "_" (Text.pack . show . typeRep $ Proxy @a)
    fileName = prefix <> Text.unpack typeName <> ".json"
    aesonValueGenerator seed = Text.lines . encodeGolden seed $ genSamples seed gen
    aesonValueReader t =
      either (Left . Text.pack) (const $ Right ()) $
        Aeson.eitherDecodeStrict (Text.encodeUtf8 t) >>= decodeGolden @a
  fileExists <- liftIO $ doesFileExist fileName
  pure $ if fileExists then
    ExistingFile fileName aesonValueGenerator (Just aesonValueReader)
  else
    NewFile fileName aesonValueGenerator

encodeGolden :: ToJSON a => Seed -> Seq a -> Text
encodeGolden seed samples =
  let
    aesonSeed (Seed value gamma) =
      Aeson.object [ "value" .= value, "gamma" .= gamma ]

    encodePretty =
      Text.decodeUtf8 . ByteString.toStrict . encodePretty' defConfig
        { confIndent = Spaces 2
        , confCompare = compare
        }
  in
    encodePretty $
      Aeson.object [ "seed" .= aesonSeed seed, "samples" .= Aeson.toJSON samples ]

decodeSeed :: Text -> Either String Seed
decodeSeed text =
  let
    getSeed :: Aeson.Object -> Either String Seed
    getSeed =
      Aeson.parseEither $ \obj -> do
        value <- obj .: "seed" >>= (.: "value")
        gamma <- obj .: "seed" >>= (.: "gamma")
        pure $ Seed value gamma
  in
    Aeson.eitherDecodeStrict (Text.encodeUtf8 text) >>= getSeed

decodeGolden :: FromJSON a => Aeson.Object -> Either String (Seed, Seq a)
decodeGolden = Aeson.parseEither $ \obj -> do
  value <- obj .: "seed" >>= (.: "value")
  gamma <- obj .: "seed" >>= (.: "gamma")
  samples <- obj .: "samples"
  pure (Seed value gamma, samples)
