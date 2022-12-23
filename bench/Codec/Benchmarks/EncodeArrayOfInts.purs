module Codec.Benchmarks.EncodeArrayOfInts where

import Prelude

import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, tags)
import Codec.Codec as Codec
import Codec.Json.Bidirectional.Class as BiC
import Codec.Json.Bidirectional.Value as BiV
import Codec.Json.Unidirectional.Encode.Class as UniCE
import Codec.Json.Unidirectional.Encode.Value as UniV
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Data.Function.Uncurried (runFn2)
import Foreign (Foreign)
import Foreign.ReadWrite as FRW
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)
import Unsafe.Coerce (unsafeCoerce)

benchmark :: BenchProps -> Benchmark
benchmark props = mkBenchmark
  { slug: "encode--array-of-ints" <> props.pathOptimizedStr
  , title: "Encoding an array of ints" <> props.titleOptimizedStr
  , sizes: 1 Array... 5 <#> (_ * 1_000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ argonaut
      , foreignReadWrite
      , jsonCodecsUniValue
      , jsonCodecsUniClass
      , codecArgonaut
      , jsonCodecsBiValue
      , jsonCodecsBiClass
      , baseline
      ]
  }

argonaut :: BenchmarkFunction (Array Int)
argonaut = benchFnTagged "argonaut-codec"
  (tags [ "unidirectional", "top3" ])
  AE.encodeJson

foreignReadWrite ∷ BenchmarkFunction (Array Int)
foreignReadWrite =
  benchFnTagged "foreign-readwrite"
    (tags [ "unidirectional", "top3" ])
    $ (unsafeCoerce :: (Array Int -> Foreign) -> (_ -> Json)) FRW.writeForeign

jsonCodecsUniValue :: BenchmarkFunction (Array Int)
jsonCodecsUniValue =
  benchFnTagged "json-codecs - uni - value"
    (tags [ "unidirectional", "top3" ])
    $ UniV.encodeArray UniV.encodeInt

jsonCodecsUniClass :: BenchmarkFunction (Array Int)
jsonCodecsUniClass =
  benchFnTagged "json-codecs - uni - class"
    (tags [ "unidirectional" ])
    $ UniCE.encodeJson

codecArgonaut :: BenchmarkFunction (Array Int)
codecArgonaut =
  benchFnTagged "codec-argonaut"
    (tags [ "bidirectional", "bidirectional-value" ])
    $ CC.encode
    $ CA.array CA.int

jsonCodecsBiValue :: BenchmarkFunction (Array Int)
jsonCodecsBiValue =
  benchFnTagged "json-codecs - bidi - value"
    (tags [ "bidirectional", "bidirectional-value" ])
    $ \a -> runFn2 (Codec.encoder $ BiV.array BiV.int) unit a

jsonCodecsBiClass :: BenchmarkFunction (Array Int)
jsonCodecsBiClass =
  benchFnTagged "json-codecs - bidi - class"
    (tags [ "bidirectional" ])
    $ \a -> runFn2 (Codec.encoder BiC.codecJson) unit a

baseline :: BenchmarkFunction (Array Int)
baseline = benchFnTagged "unsafeCoerce"
  (tags [ "unidirectional", "top3", "bidirectional", "bidirectional-value" ])
  unsafeCoerce
