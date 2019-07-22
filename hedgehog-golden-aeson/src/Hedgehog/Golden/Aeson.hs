module Hedgehog.Golden.Aeson
  ( goldenProperty
  ) where

import           Prelude hiding (writeFile)

import           Control.Monad.Extra (ifM)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON, (.=))
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import           Data.ByteString.Lazy (ByteString, writeFile)
import           Data.Proxy (Proxy(..))
import           Data.Typeable (Typeable, typeRep)
import           Hedgehog (Gen, Seed(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Range as Range
import           System.Directory (doesFileExist, createDirectoryIfMissing)

goldenProperty :: forall a m
   . Typeable a
  => MonadIO m
  => ToJSON a
  => Gen a -> m ()
goldenProperty gen =
  let
    typeName = show . typeRep $ Proxy @a
    fileName = "./golden/" <> typeName <> ".json"
    fileExists = liftIO . doesFileExist
    compareExisting = undefined
  in
    ifM
      (fileExists fileName)
      (compareExisting fileName gen)
      (createNewFile fileName gen)

createNewFile :: MonadIO m => ToJSON a => FilePath -> Gen a -> m ()
createNewFile fp gen = Seed.random >>= \seed ->
  let
    Just samples =
      Gen.evalGen 10 seed $
        Aeson.toJSON <$> Gen.seq (Range.singleton 10) gen
    object = Aeson.object
      [ "seed" .= aesonSeed seed
      , "samples" .= Tree.treeValue samples
      ]
  in liftIO $ do
    createDirectoryIfMissing True "golden"
    writeFile fp (encodePretty object)

aesonSeed :: Seed -> Aeson.Value
aesonSeed (Seed value gamma) =
  Aeson.object [ "value" .= value, "gamma" .= gamma ]

encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' defConfig
  { confIndent = Spaces 2
  , confCompare = compare
  }
