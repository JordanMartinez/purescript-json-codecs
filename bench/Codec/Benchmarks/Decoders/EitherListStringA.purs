module Codec.Benchmarks.Decoders.EitherListStringA where

import Prelude

import Data.Argonaut.Core (Json, caseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List (List(..))
import Data.TraversableWithIndex (traverseWithIndex)

toNumber :: Json -> Either (List String) Number
toNumber json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Number but got Null")
    (\_ -> Left $ pure "Expected a value of type Number but got Boolean")
    pure
    (\_ -> Left $ pure "Expected a value of type Number but got String")
    (\_ -> Left $ pure "Expected a value of type Number but got Array")
    (\_ -> Left $ pure "Expected a value of type Number but got Object")
    json

toJArray :: Json -> Either (List String) (Array Json)
toJArray json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Array but got Null")
    (\_ -> Left $ pure "Expected a value of type Array but got Boolean")
    (\_ -> Left $ pure "Expected a value of type Array but got Number")
    (\_ -> Left $ pure "Expected a value of type Array but got String")
    pure
    (\_ -> Left $ pure "Expected a value of type Array but got Object")
    json

toInt :: Json -> Either (List String) Int
toInt = toNumber >=> (\n -> note (pure $ "could not convert Number to Int: " <> show n) $ Int.fromNumber n)

toInt_noShow :: Json -> Either (List String) Int
toInt_noShow = toNumber >=> (\n -> note (pure $ "could not convert Number to Int") $ Int.fromNumber n)

withIndex :: forall a. Int -> Either (List String) a -> Either (List String) a
withIndex idx = lmap (Cons $ "[" <> show idx <> "]")

toArray :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> withIndex i $ toElem j)

toArray_noWithIndex :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Array a)
toArray_noWithIndex toElem = toJArray >=> traverseWithIndex (\i j -> lmap (Cons $ "[" <> show i <> "]") $ toElem j)
