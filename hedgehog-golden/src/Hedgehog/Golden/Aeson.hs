module Hedgehog.Golden.Aeson
  ( encodeGolden
  , decodeGolden
  ) where

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Hedgehog (Seed(..))

encodeGolden :: ToJSON a => Seed -> Seq a -> Text
encodeGolden seed samples = encodePretty $
  Aeson.object [ "seed" .= aesonSeed seed, "samples" .= Aeson.toJSON samples ]

aesonSeed :: Seed -> Aeson.Value
aesonSeed (Seed value gamma) =
  Aeson.object [ "value" .= value, "gamma" .= gamma ]

encodePretty :: ToJSON a => a -> Text
encodePretty = Text.decodeUtf8 . ByteString.toStrict . encodePretty' defConfig
  { confIndent = Spaces 2
  , confCompare = compare
  }

decodeGolden :: FromJSON a => Aeson.Object -> Either String (Seed, Seq a)
decodeGolden = Aeson.parseEither $ \obj -> do
  value <- obj .: "seed" >>= (.: "value")
  gamma <- obj .: "seed" >>= (.: "gamma")
  samples <- obj .: "samples"
  pure (Seed value gamma, samples)
