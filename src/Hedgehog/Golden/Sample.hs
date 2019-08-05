module Hedgehog.Golden.Sample
  ( genSamples
  ) where

import           Prelude

import           Data.Functor.Identity (runIdentity)
import           Data.Sequence (Seq)
import           Hedgehog
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Range as Range
import           Control.Monad.Trans.Maybe (MaybeT(..))

-- | Generate a fixed Seq of @a@ from the given generator
genSamples :: Seed -> Gen a -> Seq a
genSamples seed gen =
  let
    loop n = \case
      Just tree -> tree
      Nothing ->
        if n < 0 then
          error "Too many discards - abandoned generating samples"
        else
          loop (n - 1)
            . fmap Tree.nodeValue
            . runIdentity
            . runMaybeT
            . Tree.runTree
            . Gen.runGenT 0 seed $ Gen.seq (Range.singleton 10) gen
  in
    loop (100 :: Int) Nothing
