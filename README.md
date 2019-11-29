Hedgehog Golden
===============
A golden file extension to hedgehog

Example
-------

```haskell
{-# LANGUAGE TemplateHaskell #-}

import           Hedeghog
import qualified Hedeghog.Gen as Gen
import qualified Hedeghog.Golden.Aeson as Aeson

-- | A golden test for characters in the hex range
prop_char_golden :: Property
prop_char_golden = Aeson.goldenProperty Gen.hexit

tests :: IO Bool
tests = checkParallel $$discover
```
