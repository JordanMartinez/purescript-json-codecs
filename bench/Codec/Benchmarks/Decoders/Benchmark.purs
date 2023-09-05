module Codec.Benchmarks.Decoders.Benchmark where

import Prelude

import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Decoders.EitherErrA as EitherErrA
import Codec.Benchmarks.Decoders.EitherListStringA as EitherListStringA
import Codec.Benchmarks.Decoders.EitherStringA as EitherStringA
import Codec.Benchmarks.Decoders.MaybeA as MaybeA
import Codec.Benchmarks.Utils (BenchProps, tags)
import Data.Argonaut.Core (Json)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)
import Unsafe.Coerce (unsafeCoerce)

benchmark :: BenchProps -> Benchmark
benchmark props = mkBenchmark
  { slug: "decode--approaches--array-of-ints" <> props.pathOptimizedStr
  , title: "Benchmarking decode approaches" <> props.titleOptimizedStr
  , sizes: [ 1_000, 3_000, 5_000 ]
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> encodeArrayInt $ vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ eitherStringA
      , eitherListStringA
      , eitherErrA
      , maybeA
      ]
  }
  where
  encodeArrayInt :: Gen (Array Int) -> Gen Json
  encodeArrayInt = unsafeCoerce

eitherStringA :: BenchmarkFunction Json
eitherStringA =
  benchFnTagged "Either-String-A"
    (tags [])
    $ \j -> EitherStringA.toArray EitherStringA.toInt j

eitherListStringA :: BenchmarkFunction Json
eitherListStringA =
  benchFnTagged "Either-List-String-A"
    (tags [])
    $ \j -> EitherListStringA.toArray EitherListStringA.toInt j

eitherErrA :: BenchmarkFunction Json
eitherErrA =
  benchFnTagged "Either-DecodeErr-A"
    (tags [])
    $ \j -> EitherErrA.toArray EitherErrA.toInt j

maybeA :: BenchmarkFunction Json
maybeA =
  benchFnTagged "Maybe-A"
    (tags [])
    $ \j -> MaybeA.toArray MaybeA.toInt j
