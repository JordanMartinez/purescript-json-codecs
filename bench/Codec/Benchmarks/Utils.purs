module Codec.Benchmarks.Utils where

import Prelude

import Benchotron.Core (Benchmark, unpackBenchmark)
import Benchotron.UI.Console (benchmarkToFile)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Node.FS.Sync (exists, mkdir)
import Node.Path as Path
import Node.Process (lookupEnv)

type BenchProps =
  { titleOptimizedStr :: String
  , pathOptimizedStr :: String
  }

benchIt :: (BenchProps -> Benchmark) -> Effect Unit
benchIt mkBench = do
  optimized <- isOptimized
  let
    titleOptimizedStr
      | optimized = " (optimized)"
      | otherwise = " (unoptimized)"
    pathOptimizedStr
      | optimized = "_optimized"
      | otherwise = "_unoptimized"
    b = mkBench { titleOptimizedStr, pathOptimizedStr }
    slug = unpackBenchmark _.slug b
    basePath = Path.concat [ "bench", "results" ]
  unlessM (exists basePath) do
    mkdir basePath
  benchmarkToFile (Path.concat [ basePath, slug <> ".json" ]) b

isOptimized :: Effect Boolean
isOptimized = isJust <$> lookupEnv "OPTIMIZED"

tags :: Array String -> Set String
tags = Set.fromFoldable
