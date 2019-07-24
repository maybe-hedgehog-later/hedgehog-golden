module Hedgehog.Golden.Aeson
  ( goldenProperty
  ) where

import           Prelude hiding (writeFile)

import           Control.Monad.Extra (ifM)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import           Data.Algorithm.Diff (Diff(..), getDiff)
import           Data.ByteString.Lazy (ByteString, toStrict, writeFile)
import           Data.Functor ((<&>))
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable, typeRep)
import           GHC.Stack (HasCallStack)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Range as Range
import           System.Directory (doesFileExist, createDirectoryIfMissing)

goldenProperty :: forall a
   . HasCallStack
  => Typeable a
  => FromJSON a
  => ToJSON a
  => Eq a
  => Gen a -> Property
goldenProperty gen =
  let
    typeName = show . typeRep $ Proxy @a
    fileName = "./golden/" <> typeName <> ".json"
    fileExists = liftIO . doesFileExist
  in
    withTests 1 . property $ ifM
      (fileExists fileName)
      (compareExisting fileName gen)
      (createNewFile fileName gen)

createNewFile :: MonadTest m => MonadIO m => ToJSON a => FilePath -> Gen a -> m ()
createNewFile fp gen = Seed.random >>= \seed ->
  let
    samples =
      Aeson.toJSON <$> genSamples seed gen
    object =
      Aeson.object [ "seed" .= aesonSeed seed, "samples" .= samples ]
  in liftIO $ do
    createDirectoryIfMissing True "golden"
    writeFile fp (encodePretty object)

genSamples :: Seed -> Gen a -> Seq a
genSamples seed gen =
  let
    loop n = \case
      Just tree -> Tree.treeValue tree
      Nothing ->
        if n < 0 then
          error "Too many discards - abandoned generating samples"
        else
          loop (n - 1) . Gen.evalGen 0 seed $ Gen.seq (Range.singleton 10) gen
  in
    loop (100 :: Int) Nothing

aesonSeed :: Seed -> Aeson.Value
aesonSeed (Seed value gamma) =
  Aeson.object [ "value" .= value, "gamma" .= gamma ]

encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' defConfig
  { confIndent = Spaces 2
  , confCompare = compare
  }

compareExisting :: MonadTest m => MonadIO m => Eq a => ToJSON a => FromJSON a => FilePath -> Gen a -> m ()
compareExisting fileName gen =
  liftIO (Aeson.eitherDecodeFileStrict fileName) >>=
  getSeedAndElements >>=
  uncurry (compareWith gen)

getSeedAndElements :: MonadTest m => FromJSON a => Either String Aeson.Object -> m (Seed, Seq a)
getSeedAndElements = either fail pure . (=<<) decodeGoldenJson

decodeGoldenJson :: FromJSON a => Aeson.Object -> Either String (Seed, Seq a)
decodeGoldenJson = Aeson.parseEither $ \obj -> do
  value <- obj .: "seed" >>= (.: "value")
  gamma <- obj .: "seed" >>= (.: "gamma")
  samples <- obj .: "samples"
  pure (Seed value gamma, samples)

compareWith :: MonadTest m => ToJSON a => Eq a => Gen a -> Seed -> Seq a -> m ()
compareWith gen seed samplesFromFile =
  let
    newSamples = genSamples seed gen
    jsonDiff = diffMessage samplesFromFile newSamples
  in
    if newSamples == samplesFromFile then
      pure ()
    else
      Property.failWith Nothing jsonDiff


diffMessage :: ToJSON a => a -> a -> String
diffMessage expected actual =
  let
    toLines = Text.lines . Text.decodeUtf8 . toStrict . encodePretty
    diffLines = getDiff (toLines expected) (toLines actual)
  in
    unlines $ "\ESC[31;1mFile contains diff:\ESC[0m\n" : renderDiff diffLines

renderDiff :: [Diff Text] -> [String]
renderDiff diffs =
  diffs <&> \case
    Both x _ -> "    " <> Text.unpack x
    First x  -> "\ESC[31;1m  - " <> Text.unpack x <> " \ESC[0m"
    Second x -> "\ESC[32;1m  - " <> Text.unpack x <> " \ESC[0m"
