module Codec.Benchmarks.Decoders.MaybeA where

import Prelude

import Data.Argonaut.Core (Json, caseJson)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

toNumber :: Json -> Maybe Number
toNumber json =
  caseJson
    (\_ -> Nothing)
    (\_ -> Nothing)
    pure
    (\_ -> Nothing)
    (\_ -> Nothing)
    (\_ -> Nothing)
    json

toJArray :: Json -> Maybe (Array Json)
toJArray json =
  caseJson
    (\_ -> Nothing)
    (\_ -> Nothing)
    (\_ -> Nothing)
    (\_ -> Nothing)
    pure
    (\_ -> Nothing)
    json

toInt :: Json -> Maybe Int
toInt = toNumber >=> Int.fromNumber

toArray :: forall a. (Json -> Maybe a) -> Json -> Maybe (Array a)
toArray toElem = toJArray >=> traverse toElem
