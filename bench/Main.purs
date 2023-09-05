module Main where

import Prelude

import Codec.Benchmarks.DecodeArrayOfInts as DecodeArrayOfInts
import Codec.Benchmarks.EncodeArrayOfInts as EncodeArrayOfInts
import Codec.Benchmarks.Decoders.Benchmark as DecoderApproaches
import Codec.Benchmarks.Utils (benchIt)
import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (traverse_)
import Effect (Effect)

main :: Effect Unit
main = traverse_ benchIt $ Array.catMaybes
  [ DecoderApproaches.benchmark <$ guard true
  , EncodeArrayOfInts.benchmark <$ guard true
  , DecodeArrayOfInts.benchmark <$ guard true
  ]
