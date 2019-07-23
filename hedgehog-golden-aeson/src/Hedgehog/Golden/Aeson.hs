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
import           Data.ByteString.Lazy (ByteString, writeFile)
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Typeable (Typeable, typeRep)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Range as Range
import           System.Directory (doesFileExist, createDirectoryIfMissing)

goldenProperty :: forall a
   . HasCallStack
  => Typeable a
  => FromJSON a
  => ToJSON a
  => Eq a
  => Show a
  => Gen a -> Property
goldenProperty gen =
  let
    typeName = show . typeRep $ Proxy @a
    fileName = "./golden/" <> typeName <> ".json"
    fileExists = liftIO . doesFileExist
  in
    withFrozenCallStack . property $ ifM
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

compareExisting :: MonadTest m => MonadIO m => Show a => Eq a => FromJSON a => FilePath -> Gen a -> m ()
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

compareWith :: MonadTest m => Show a => Eq a => Gen a -> Seed -> Seq a -> m ()
compareWith gen seed samplesFromFile =
  let
    newSamples = genSamples seed gen
  in
    newSamples === samplesFromFile
