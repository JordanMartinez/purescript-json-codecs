module Codec.Benchmarks.DecodeADTs where

import Prelude

import Benchotron.Core (Benchmark, BenchmarkFunction, benchFnTagged, mkBenchmark)
import Codec.Benchmarks.Utils (BenchProps, tags)
import Codec.Json.Unidirectional.Value as UniV
import Control.Alt ((<|>))
import Control.Monad.Gen (chooseInt)
import Control.Monad.Gen.Common (genEither, genMaybe, genTuple)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode as AD
import Data.Array as Array
import Data.Codec as CC
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Compat as CACompat
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.Gen (genAlphaLowercaseString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JSON (JSON)
import JSON as JSON
import JSON.Array as JA
import JSON.Object as JO
import Test.QuickCheck.Gen (Gen, vectorOf)
import Unsafe.Coerce (unsafeCoerce)

type ADT =
  Either
    ( Tuple
        (Maybe String)
        ( Array
            ( Either
                ( Either
                    Int
                    { foo :: String, bar :: Int }
                )
                { baz :: Set String }
            )
        )
    )
    (String)

genADT :: Int -> Gen ADT
genADT n = do
  genEither
    ( genTuple
        (genMaybe genAlphaLowercaseString)
        ( vectorOf n $ genEither
            ( genEither
                (chooseInt 0 n)
                ( { foo: _, bar: _ }
                    <$> genAlphaLowercaseString
                    <*> chooseInt 0 n
                )
            )
            (map ({ baz: _ } <<< Set.fromFoldable) $ vectorOf n genAlphaLowercaseString)
        )
    )
    genAlphaLowercaseString

fromADT :: ADT -> Json
fromADT =
  UniV.fromEitherTagged
    ( UniV.fromTuple
        (UniV.fromNullNothingOrJust UniV.fromString)
        ( UniV.fromArray $ UniV.fromEitherTagged
            ( UniV.fromEitherTagged
                UniV.fromInt
                ( UniV.fromRecord
                    { foo: UniV.fromRequired UniV.fromString
                    , bar: UniV.fromRequired UniV.fromInt
                    }
                )
            )
            ( UniV.fromRecord
                { baz: UniV.fromRequired $ UniV.fromSet UniV.fromString
                }
            )
        )
    )
    UniV.fromString

toADT :: Json -> Either UniV.DecodeError ADT
toADT =
  UniV.toEitherTagged
    ( UniV.toTuple
        (UniV.toNullNothingOrJust UniV.toString)
        ( UniV.toArray $ UniV.toEitherTagged
            ( UniV.toEitherTagged
                UniV.toInt
                ( UniV.toRecord
                    { foo: UniV.toRequired UniV.toString
                    , bar: UniV.toRequired UniV.toInt
                    }
                )
            )
            ( UniV.toRecord
                { baz: UniV.toRequired $ UniV.toSet UniV.toString
                }
            )
        )
    )
    UniV.toString

benchmark :: BenchProps -> Benchmark
benchmark props = mkBenchmark
  { slug: "decode--nested-adt" <> props.pathOptimizedStr
  , title: "Decoding a nested ADT" <> props.titleOptimizedStr
  , sizes: 1 Array... 5 <#> (_ * 50)
  , sizeInterpretation: "Number used in various generators"
  , inputsPerSize: 1
  , gen: \n -> fromADT <$> genADT n
  , functions:
      [ argonaut
      , json
      , jsonCodecs
      , codecArgonaut
      ]
  }

tagJsonCodecWithArgonautCodec = "json-codec-with-argonaut-codec" :: String
tagJsonCodecWithJson = "json-codec-with-json" :: String
tagJsonCodecWithCodecArgonaut = "json-codec-with-codec-argonaut" :: String

argonaut :: BenchmarkFunction Json
argonaut = benchFnTagged "argonaut-codec"
  (tags [ tagJsonCodecWithArgonautCodec ])
  (AD.decodeJson :: Json -> _ _ ADT)

json :: BenchmarkFunction Json
json = benchFnTagged "json"
  (tags [ tagJsonCodecWithJson ])
  (toJSON >>> toAdt)
  where
  toJSON :: Json -> JSON
  toJSON = unsafeCoerce

  toEitherTagged :: forall a b. (JSON -> Maybe a) -> (JSON -> Maybe b) -> JSON -> Maybe (Either a b)
  toEitherTagged toLeft toRight = JSON.toJObject >=> \jo -> do
    tag <- JO.lookup "tag" jo >>= JSON.toString
    case tag of
      "Left" -> JO.lookup "value" jo >>= toLeft >>> map Left
      "Right" -> JO.lookup "value" jo >>= toRight >>> map Right
      _ -> Nothing

  toMaybeNullable :: forall a. (JSON -> Maybe a) -> JSON -> Maybe (Maybe a)
  toMaybeNullable toA j = (Nothing <$ JSON.toNull j) <|> (Just <$> toA j)

  toTuple :: forall a b. (JSON -> Maybe a) -> (JSON -> Maybe b) -> JSON -> Maybe (Tuple a b)
  toTuple toA toB = JSON.toJArray >=> \ja -> do
    Tuple
      <$> (JA.index ja 0 >>= toA)
      <*> (JA.index ja 1 >>= toB)

  toAdt = toEitherTagged
    ( toTuple
        (toMaybeNullable JSON.toString)
        ( JSON.toArray >=> traverse
            ( toEitherTagged
                ( toEitherTagged
                    JSON.toInt
                    ( JSON.toJObject >=> \jo -> do
                        { foo: _, bar: _ }
                          <$> (JO.lookup "foo" jo >>= JSON.toString)
                          <*> (JO.lookup "bar" jo >>= JSON.toInt)
                    )
                )
                ( JSON.toJObject >=> \jo -> do
                    { bar: _ } <$>
                      ( JO.lookup "baz" jo >>= \bazJson ->
                          JSON.toArray bazJson >>= traverse JSON.toString >>> map Set.fromFoldable
                      )
                )
            )
        )
    )
    JSON.toString

jsonCodecs :: BenchmarkFunction Json
jsonCodecs =
  benchFnTagged "json-codecs"
    (tags [ tagJsonCodecWithArgonautCodec, tagJsonCodecWithJson, tagJsonCodecWithCodecArgonaut ])
    toADT

codecArgonaut :: BenchmarkFunction Json
codecArgonaut =
  benchFnTagged "codec-argonaut"
    (tags [ tagJsonCodecWithCodecArgonaut ])
    $ CC.decode
    $ CAC.either
        ( CAC.tuple
            (CACompat.maybe CA.string)
            ( CA.array
                ( CAC.either
                    ( CAC.either
                        CA.int
                        ( CAR.object "FooBarRecord"
                            { foo: CA.string
                            , bar: CA.int
                            }
                        )
                    )
                    ( CAR.object "BarRecord"
                        { baz: CAC.set CA.string }
                    )
                )
            )
        )
        CA.string

