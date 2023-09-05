module Codec.Benchmarks.DecodeArrayOfInts where

import Prelude

import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, tags)
import Codec.Json.Unidirectional.Value as UniV
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode as AD
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Data.Traversable (traverse)
import JSON (JSON)
import JSON as JSON
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)
import Unsafe.Coerce (unsafeCoerce)

benchmark :: BenchProps -> Benchmark
benchmark props = mkBenchmark
  { slug: "decode--array-of-ints" <> props.pathOptimizedStr
  , title: "Decoding an array of ints" <> props.titleOptimizedStr
  , sizes: 1 Array... 5 <#> (_ * 1_000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> encodeArrayInt $ vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ argonaut
      , json
      , jsonCodecs
      , codecArgonaut
      ]
  }
  where
  encodeArrayInt :: Gen (Array Int) -> Gen Json
  encodeArrayInt = unsafeCoerce

tagJsonCodecWithArgonautCodec = "json-codec-with-argonaut-codec" :: String
tagJsonCodecWithJson = "json-codec-with-json" :: String
tagJsonCodecWithCodecArgonaut = "json-codec-with-codec-argonaut" :: String
tagJsonCodecWithBaseline = "json-codec-with-baseline" :: String

argonaut :: BenchmarkFunction Json
argonaut = benchFnTagged "argonaut-codec"
  (tags [ tagJsonCodecWithArgonautCodec ])
  (AD.decodeJson :: _ -> _ _ (Array Int))

json :: BenchmarkFunction Json
json = benchFnTagged "json"
  (tags [ tagJsonCodecWithJson ])
  ((unsafeCoerce :: Json -> JSON) >>> JSON.toArray >=> traverse JSON.toInt)

jsonCodecs :: BenchmarkFunction Json
jsonCodecs =
  benchFnTagged "json-codecs"
    (tags [ tagJsonCodecWithArgonautCodec, tagJsonCodecWithJson, tagJsonCodecWithCodecArgonaut, tagJsonCodecWithBaseline ])
    $ \j -> UniV.toArray UniV.toInt j

codecArgonaut :: BenchmarkFunction Json
codecArgonaut =
  benchFnTagged "codec-argonaut"
    (tags [ tagJsonCodecWithCodecArgonaut ])
    $ CC.decode
    $ CA.array CA.int

