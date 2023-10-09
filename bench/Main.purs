module Main where

import Prelude

import Codec.Benchmarks.DecodeArrayOfInts as DecodeArrayOfInts
import Codec.Benchmarks.DecodeADTs as DecodeADTs
import Codec.Benchmarks.Decoders.Benchmark as DecoderApproaches
import Codec.Benchmarks.EncodeArrayOfInts as EncodeArrayOfInts
import Codec.Benchmarks.Utils (benchIt)
import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (traverse_)
import Effect (Effect)

main :: Effect Unit
main = traverse_ benchIt $ Array.catMaybes
  [ DecoderApproaches.microBenchmark <$ guard false
  , DecoderApproaches.approachBenchmark <$ guard false
  , EncodeArrayOfInts.benchmark <$ guard false
  , DecodeArrayOfInts.benchmark <$ guard false
  , DecodeADTs.benchmark <$ guard true
  ]
