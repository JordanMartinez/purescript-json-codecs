module Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark, unpackBenchmark)
import Benchotron.UI.Console (benchmarkToFile)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Foldable (foldr, traverse_)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Node.Path as Path
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)

main :: Effect Unit
main = traverse_ benchIt
  [ benchSum ]

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

benchSum :: Benchmark
benchSum = mkBenchmark
  { slug: "array-of-ints"
  , title: "Finding the sum of an array"
  , sizes: 1 Array... 5 <#> (_ * 1_000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (arbitrary :: Gen Int)
  , functions:
      [ benchFn "foldr" Array.length
      ]
  }
