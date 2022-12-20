module Main where

import Prelude

import Codec.Benchmarks.Utils (benchIt)
import Codec.Benchmarks.ArrayOfInts as ArrayOfInts
import Data.Foldable (traverse_)
import Effect (Effect)

main :: Effect Unit
main = traverse_ benchIt
  [ ArrayOfInts.benchmark ]
