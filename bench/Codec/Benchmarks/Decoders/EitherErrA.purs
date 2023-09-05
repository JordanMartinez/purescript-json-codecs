-- @inline export withIndex arity=1
module Codec.Benchmarks.Decoders.EitherErrA where

import Prelude

import Data.Argonaut.Core (Json, caseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.TraversableWithIndex (traverseWithIndex)

data DecodeErr
  = AtKey String DecodeErr
  | AtIndex Int DecodeErr
  | DecodeErr String

toNumber :: Json -> Either DecodeErr Number
toNumber json =
  caseJson
    (\_ -> Left $ DecodeErr "Expected a value of type Number but got Null")
    (\_ -> Left $ DecodeErr "Expected a value of type Number but got Boolean")
    pure
    (\_ -> Left $ DecodeErr "Expected a value of type Number but got String")
    (\_ -> Left $ DecodeErr "Expected a value of type Number but got Array")
    (\_ -> Left $ DecodeErr "Expected a value of type Number but got Object")
    json

toJArray :: Json -> Either DecodeErr (Array Json)
toJArray json =
  caseJson
    (\_ -> Left $ DecodeErr "Expected a value of type Array but got Null")
    (\_ -> Left $ DecodeErr "Expected a value of type Array but got Boolean")
    (\_ -> Left $ DecodeErr "Expected a value of type Array but got Number")
    (\_ -> Left $ DecodeErr "Expected a value of type Array but got String")
    pure
    (\_ -> Left $ DecodeErr "Expected a value of type Array but got Object")
    json

toInt :: Json -> Either DecodeErr Int
toInt = toNumber >=> (\n -> note (DecodeErr $ "could not convert Number to Int: " <> show n) $ Int.fromNumber n)

toInt_noShow :: Json -> Either DecodeErr Int
toInt_noShow = toNumber >=> (\n -> note (DecodeErr $ "could not convert Number to Int") $ Int.fromNumber n)

withIndex :: forall a. Int -> Either DecodeErr a -> Either DecodeErr a
withIndex idx = lmap (AtIndex idx)

toArray :: forall a. (Json -> Either DecodeErr a) -> Json -> Either DecodeErr (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> withIndex i $ toElem j)

toArray_noWithIndex :: forall a. (Json -> Either DecodeErr a) -> Json -> Either DecodeErr (Array a)
toArray_noWithIndex toElem = toJArray >=> traverseWithIndex (\i j -> lmap (AtIndex i) $ toElem j)
