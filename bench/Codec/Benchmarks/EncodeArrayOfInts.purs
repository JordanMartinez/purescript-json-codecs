module Codec.Benchmarks.EncodeArrayOfInts where

import Prelude

import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, tags)
import Codec.Json.Unidirectional.Value as UniV
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Foreign (Foreign)
import Foreign.ReadWrite as FRW
import JSON as JSON
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
      , jsonCodecs
      , json
      , codecArgonaut
      , baseline
      ]
  }

tagJsonCodecWithArgonautCodec = "json-codec-with-argonaut-codec" :: String
tagJsonCodecWithJson = "json-codec-with-gary-json" :: String
tagJsonCodecWithForeignReadWrite = "json-codec-with-foreign-readwrite" :: String
tagJsonCodecWithCodecArgonaut = "json-codec-with-codec-argonaut" :: String
tagJsonCodecWithBaseline = "json-codec-with-baseline" :: String

argonaut :: BenchmarkFunction (Array Int)
argonaut = benchFnTagged "argonaut-codec"
  (tags [ tagJsonCodecWithArgonautCodec ])
  AE.encodeJson

json :: BenchmarkFunction (Array Int)
json = benchFnTagged "garyb-json"
  (tags [ tagJsonCodecWithJson ])
  (map JSON.fromInt >>> JSON.fromArray)

foreignReadWrite ∷ BenchmarkFunction (Array Int)
foreignReadWrite =
  benchFnTagged "foreign-readwrite"
    (tags [ tagJsonCodecWithForeignReadWrite ])
    $ (unsafeCoerce :: (Array Int -> Foreign) -> (_ -> Json)) FRW.writeForeign

jsonCodecs :: BenchmarkFunction (Array Int)
jsonCodecs =
  benchFnTagged "json-codecs"
    (tags [ tagJsonCodecWithArgonautCodec, tagJsonCodecWithJson, tagJsonCodecWithForeignReadWrite, tagJsonCodecWithCodecArgonaut, tagJsonCodecWithBaseline ])
    $ UniV.fromArray UniV.fromInt

codecArgonaut :: BenchmarkFunction (Array Int)
codecArgonaut =
  benchFnTagged "codec-argonaut"
    (tags [ tagJsonCodecWithCodecArgonaut ])
    $ CC.encode
    $ CA.array CA.int

baseline :: BenchmarkFunction (Array Int)
baseline = benchFnTagged "unsafeCoerce"
  (tags [ tagJsonCodecWithBaseline ])
  unsafeCoerce
