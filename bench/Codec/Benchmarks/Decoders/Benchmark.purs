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

microBenchmark :: BenchProps -> Benchmark
microBenchmark props = mkBenchmark
  { slug: "decode--minute-changes--array-of-ints" <> props.pathOptimizedStr
  , title: "Minute changes in decode approach" <> props.titleOptimizedStr
  , sizes: [ 1_000, 3_000, 5_000 ]
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> encodeArrayInt $ vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ eitherStringA
      , eitherStringA_noShow
      , eitherStringA_noWithIndex
      , eitherStringA_noShow_noWithIndex
      , maybeA
      ]
  }
  where
  encodeArrayInt :: Gen (Array Int) -> Gen Json
  encodeArrayInt = unsafeCoerce

approachBenchmark :: BenchProps -> Benchmark
approachBenchmark props = mkBenchmark
  { slug: "decode--approaches--array-of-ints" <> props.pathOptimizedStr
  , title: "Decode approaches" <> props.titleOptimizedStr
  , sizes: [ 1_000, 3_000, 5_000 ]
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> encodeArrayInt $ vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ eitherStringA_noShow_noWithIndex
      , eitherListStringA_noShow_noWithIndex
      , eitherErrA_noShow_noWithIndex
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

eitherStringA_noShow :: BenchmarkFunction Json
eitherStringA_noShow =
  benchFnTagged "Either-String-A__noShow"
    (tags [])
    $ \j -> EitherStringA.toArray EitherStringA.toInt_noShow j

eitherStringA_noWithIndex :: BenchmarkFunction Json
eitherStringA_noWithIndex =
  benchFnTagged "Either-String-A__noWithIndex"
    (tags [])
    $ \j -> EitherStringA.toArray_noWithIndex EitherStringA.toInt j

eitherStringA_noShow_noWithIndex :: BenchmarkFunction Json
eitherStringA_noShow_noWithIndex =
  benchFnTagged "Either-String-A__noShow__noWithIndex"
    (tags [])
    $ \j -> EitherStringA.toArray_noWithIndex EitherStringA.toInt_noShow j

eitherListStringA_noShow_noWithIndex :: BenchmarkFunction Json
eitherListStringA_noShow_noWithIndex =
  benchFnTagged "Either-List-String-A__noShow__noWithIndex"
    (tags [])
    $ \j -> EitherListStringA.toArray_noWithIndex EitherListStringA.toInt_noShow j

eitherErrA_noShow_noWithIndex :: BenchmarkFunction Json
eitherErrA_noShow_noWithIndex =
  benchFnTagged "Either-DecodeErr-A__noShow__noWithIndex"
    (tags [])
    $ \j -> EitherErrA.toArray_noWithIndex EitherErrA.toInt_noShow j

maybeA :: BenchmarkFunction Json
maybeA =
  benchFnTagged "Maybe-A"
    (tags [])
    $ \j -> MaybeA.toArray MaybeA.toInt j
