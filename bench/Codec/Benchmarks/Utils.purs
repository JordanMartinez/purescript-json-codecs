module Codec.Benchmarks.Utils where

import Prelude

import Benchotron.Core (Benchmark, unpackBenchmark)
import Benchotron.UI.Console (benchmarkToFile)
import Data.Foldable (foldr)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Node.Path as Path
import Node.Process (lookupEnv)

type BenchProps =
  { isOptimized :: Boolean
  , dateTimeString :: String
  }

benchIt :: (BenchProps -> Benchmark) -> Effect Unit
benchIt mkBench = do
  props <- { isOptimized: _, dateTimeString: _ }
    <$> isOptimized
    <*> dateTimeString
  let b = mkBench props
  benchmarkToFile b $ Path.concat [ "bench", unpackBenchmark _.slug b <> ".json" ]
  where
  dateTimeString = do
    map (format (yyyymmdd <> pure (Placeholder "_") <> hhmmss)) nowDateTime
    where
    inBetween :: forall a. a -> List a -> List a
    inBetween i xs = _.result $ foldr go { init: true, result: Nil } xs
      where
      go next { init, result } =
        { init: false
        , result: if init then next : Nil else Cons next (Cons i result)
        }
    yyyymmdd = inBetween (Placeholder "-") $ YearFull : MonthTwoDigits : DayOfMonthTwoDigits : Nil
    hhmmss = inBetween (Placeholder "-") $ Hours24 : MinutesTwoDigits : SecondsTwoDigits : Nil

isOptimized :: Effect Boolean
isOptimized = isJust <$> lookupEnv "OPTIMIZED"

slugify :: BenchProps -> String -> String
slugify props slug = do
  let optStr' = if props.isOptimized then "optimized_" else "unoptimized_"
  optStr' <> slug <> "_" <> props.dateTimeString
