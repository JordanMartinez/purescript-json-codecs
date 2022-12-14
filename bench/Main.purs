module Main where

import Prelude

import Codec.Benchmarks.DecodeArrayOfInts as DecodeArrayOfInts
import Codec.Benchmarks.EncodeArrayOfInts as EncodeArrayOfInts
import Codec.Benchmarks.Utils (benchIt)
import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (traverse_)
import Effect (Effect)

main :: Effect Unit
main = traverse_ benchIt $ Array.catMaybes
  [ EncodeArrayOfInts.benchmark <$ guard true
  , DecodeArrayOfInts.benchmark <$ guard true
  ]
