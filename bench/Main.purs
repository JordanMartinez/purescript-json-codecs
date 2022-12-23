module Main where

import Prelude

import Codec.Benchmarks.EncodeArrayOfInts as EncodeArrayOfInts
import Codec.Benchmarks.Utils (benchIt)
import Data.Foldable (traverse_)
import Effect (Effect)

main :: Effect Unit
main = traverse_ benchIt
  [ EncodeArrayOfInts.benchmark
  ]
