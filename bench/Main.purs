module Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark, unpackBenchmark)
import Benchotron.UI.Console (benchmarkToFile)
import Codec.Codec as Codec
import Codec.Json.Bidirectional.Class as BiC
import Codec.Json.Bidirectional.Value as BiV
import Codec.Json.Unidirectional.Encode.Class as UniCE
import Codec.Json.Unidirectional.Encode.Value as UniV
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Data.Foldable (foldr, traverse_)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function.Uncurried (runFn2)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Node.Path as Path
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)

main :: Effect Unit
main = traverse_ benchIt
  [ benchEncodeArrayInt ]

benchIt :: Benchmark -> Effect Unit
benchIt b = do
  dt <- nowDateTime
  let dt' = format (yyyymmdd <> pure (Placeholder "_") <> hhmmss) dt
  benchmarkToFile b $ Path.concat [ "bench", unpackBenchmark _.slug b <> "_" <> dt' <> ".json" ]
  where
  inBetween :: forall a. a -> List a -> List a
  inBetween i xs = _.result $ foldr go { init: true, result: Nil } xs
    where
    go next { init, result } =
      { init: false
      , result: if init then next : Nil else Cons next (Cons i result)
      }
  yyyymmdd = inBetween (Placeholder "-") $ YearFull : MonthTwoDigits : DayOfMonthTwoDigits : Nil
  hhmmss = inBetween (Placeholder ":") $ Hours24 : MinutesTwoDigits : SecondsTwoDigits : Nil

benchEncodeArrayInt :: Benchmark
benchEncodeArrayInt = mkBenchmark
  { slug: "encode--array-of-ints"
  , title: "Encoding an array of ints"
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
