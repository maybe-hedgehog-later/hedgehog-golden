module Hedgehog.Golden.Aeson
  ( encodeGolden
  , decodeGolden
  , goldenTest
  ) where

import           Prelude

import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable, typeRep)
import           Hedgehog (Gen, Seed(..))
import           Hedgehog.Golden.Sample (genSamples)
import           Hedgehog.Golden.Types (GoldenTest(..))
import           GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import           System.Directory (doesFileExist)

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

decodeGolden :: FromJSON a => Aeson.Object -> Either String (Seed, Seq a)
decodeGolden = Aeson.parseEither $ \obj -> do
  value <- obj .: "seed" >>= (.: "value")
  gamma <- obj .: "seed" >>= (.: "gamma")
  samples <- obj .: "samples"
  pure (Seed value gamma, samples)

goldenTest :: forall a. HasCallStack => Typeable a => ToJSON a => Gen a -> IO GoldenTest
goldenTest gen = withFrozenCallStack $
  let
    typeName = show . typeRep $ Proxy @a
    fileName = "golden/" <> typeName <> ".json"
    aesonValueGenerator seed = Text.lines . encodeGolden seed $ genSamples seed gen
  in do
    fileExists <- doesFileExist fileName
    pure $ if fileExists then
      ExistingFile callStack fileName aesonValueGenerator
    else
      NewFile callStack fileName aesonValueGenerator
