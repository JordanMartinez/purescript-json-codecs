module Codec.Benchmarks.ArrayOfInts where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, slugify)
import Codec.Codec as Codec
import Codec.Json.Bidirectional.Class as BiC
import Codec.Json.Bidirectional.Value as BiV
import Codec.Json.Unidirectional.Encode.Class as UniCE
import Codec.Json.Unidirectional.Encode.Value as UniV
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Data.Function.Uncurried (runFn2)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)

benchmark :: BenchProps -> Benchmark
benchmark props = mkBenchmark
  { slug: slugify props "encode--array-of-ints"
  , title: "Encoding an array of ints " <> if props.isOptimized then "(optimized)" else "(unoptimized)"
  , sizes: 1 Array... 5 <#> (_ * 1_000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ benchFn "encode (argonaut-codec)"
          AE.encodeJson
      , benchFn "encode (json-codecs - uni - value)" $
          UniV.encodeArray UniV.encodeInt
      , benchFn "encode (json-codecs - uni - class)"
          UniCE.encodeJson
      , benchFn "encode (codec-argonaut)"
          $ CC.encode
          $ CA.array CA.int
      , benchFn "encode (json-codecs - bidi - value)" $
          \a -> runFn2 (Codec.encoder $ BiV.array BiV.int) unit a
      , benchFn "encode (json-codecs - bidi - class)" $
          \a -> runFn2 (Codec.encoder BiC.codecJson) unit a
      ]
  }
