-- @inline export withIndex arity=1
module Codec.Benchmarks.Decoders.EitherStringA where

import Prelude

import Data.Argonaut.Core (Json, caseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.TraversableWithIndex (traverseWithIndex)

toNumber :: Json -> Either String Number
toNumber json =
  caseJson
    (\_ -> Left "Expected a value of type Number but got Null")
    (\_ -> Left "Expected a value of type Number but got Boolean")
    pure
    (\_ -> Left "Expected a value of type Number but got String")
    (\_ -> Left "Expected a value of type Number but got Array")
    (\_ -> Left "Expected a value of type Number but got Object")
    json

toJArray :: Json -> Either String (Array Json)
toJArray json =
  caseJson
    (\_ -> Left "Expected a value of type Array but got Null")
    (\_ -> Left "Expected a value of type Array but got Boolean")
    (\_ -> Left "Expected a value of type Array but got Number")
    (\_ -> Left "Expected a value of type Array but got String")
    pure
    (\_ -> Left "Expected a value of type Array but got Object")
    json

toInt :: Json -> Either String Int
toInt = toNumber >=> (\n -> note ("could not convert Number to Int: " <> show n) $ Int.fromNumber n)

toInt_noShow :: Json -> Either String Int
toInt_noShow = toNumber >=> (\n -> note "could not convert Number to Int" $ Int.fromNumber n)

withIndex :: forall a. Int -> Either String a -> Either String a
withIndex idx = lmap (append $ "[" <> show idx <> "]")

toArray :: forall a. (Json -> Either String a) -> Json -> Either String (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> withIndex i $ toElem j)

toArray_noWithIndex :: forall a. (Json -> Either String a) -> Json -> Either String (Array a)
toArray_noWithIndex toElem = toJArray >=> traverseWithIndex (\i j -> lmap (append ("[" <> show i <> "]")) $ toElem j)
