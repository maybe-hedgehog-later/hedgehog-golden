module Hedgehog.Golden.Sample
  ( genSamples
  ) where

import           Prelude

import           Data.Sequence (Seq)
import           Hedgehog
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Range as Range

-- | Generate a fixed Seq of @a@ from the given gnerator
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
