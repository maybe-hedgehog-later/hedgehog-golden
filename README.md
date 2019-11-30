Hedgehog Golden
===============
![](https://github.com/felixmulder/hedgehog-golden/workflows/Haskell%20CI/badge.svg)
![](https://img.shields.io/badge/Hackage-v1.0.0-blue.svg)

A golden file extension to hedgehog

Hedgehog support
----------------
[Hedgehog](http://hackage.haskell.org/package/hedgehog-1.0) v1.x series is
supported by
[hedgehog-golden](https://hackage.haskell.org/package/hedgehog-golden-1.0.0)
v1.x series

[Hedgehog](http://hackage.haskell.org/package/hedgehog-0.6.1) v0.6.x series is
supported by
[hedgehog-golden](https://hackage.haskell.org/package/hedgehog-golden-0.6.0)
v0.6.x series

Example
-------
```haskell
{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Golden.Aeson as Aeson

-- | A golden test for characters in the hex range
prop_char_golden :: Property
prop_char_golden =
  Aeson.goldenProperty Gen.hexit

tests :: IO Bool
tests =
  checkParallel $$discover
```
