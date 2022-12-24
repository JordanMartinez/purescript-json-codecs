module Codec.Benchmarks.DecodeArrayOfInts where

import Prelude

import Ansi.Codes (GraphicsParam)
import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, tags)
import Codec.Codec as Codec
import Codec.Json.Bidirectional.Class as BiC
import Codec.Json.Bidirectional.Value as BiV
import Codec.Json.Errors.AnsiDodoError (runJsonDecoderADE)
import Codec.Json.JsonCodec (JsonCodec)
import Codec.Json.JsonDecoder (JsonDecoder)
import Codec.Json.Unidirectional.Decode.Class as UniC
import Codec.Json.Unidirectional.Decode.Value as UniV
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode as AD
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Dodo (Doc)
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
      , jsonCodecsUniValue
      , jsonCodecsUniClass
      , codecArgonaut
      , jsonCodecsBiValue
      , jsonCodecsBiClass
      , baseline
      ]
  }
  where
  encodeArrayInt :: Gen (Array Int) -> Gen Json
  encodeArrayInt = unsafeCoerce

argonaut :: BenchmarkFunction Json
argonaut = benchFnTagged "argonaut-codec"
  (tags [ "unidirectional", "top3" ])
  (AD.decodeJson :: _ -> _ _ (Array Int))

jsonCodecsUniValue :: BenchmarkFunction Json
jsonCodecsUniValue =
  benchFnTagged "json-codecs - uni - value"
    (tags [ "unidirectional" ])
    $ \j -> runJsonDecoderADE j (UniV.decodeArray UniV.decodeInt)

jsonCodecsUniClass :: BenchmarkFunction Json
jsonCodecsUniClass =
  benchFnTagged "json-codecs - uni - class"
    (tags [ "unidirectional" ])
    $ \j -> runJsonDecoderADE j (UniC.decodeJson :: JsonDecoder (Doc GraphicsParam) Unit (Array Int))

codecArgonaut :: BenchmarkFunction Json
codecArgonaut =
  benchFnTagged "codec-argonaut"
    (tags [ "top3", "bidirectional", "bidirectional-value" ])
    $ CC.decode
    $ CA.array CA.int

jsonCodecsBiValue :: BenchmarkFunction Json
jsonCodecsBiValue =
  benchFnTagged "json-codecs - bidi - value"
    (tags [ "bidirectional", "bidirectional-value" ])
    $ \j -> runJsonDecoderADE j (Codec.decoder $ BiV.array BiV.int)

jsonCodecsBiClass :: BenchmarkFunction Json
jsonCodecsBiClass =
  benchFnTagged "json-codecs - bidi - class"
    (tags [ "bidirectional" ])
    $ \j -> runJsonDecoderADE j (Codec.decoder (BiC.codecJson :: JsonCodec (Doc GraphicsParam) Unit (Array Int)))

baseline :: BenchmarkFunction Json
baseline = benchFnTagged "unsafeCoerce"
  (tags [ "unidirectional", "top3", "bidirectional", "bidirectional-value" ])
  unsafeCoerce
