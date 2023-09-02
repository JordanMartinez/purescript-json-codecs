module Codec.Json.IsJsonDecoder where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Function.Uncurried (Fn2)
import Partial.Unsafe (unsafePartial)

class IsJsonDecoder :: (Type -> Type) -> Constraint
class Monad f <= IsJsonDecoder f where
  onUnrefinableValue :: forall a. String -> f a
  onTypeMismatch :: forall a. Fn2 String String (f a)
  onMissingField :: forall a. String -> f a
  onMissingIndex :: forall a. Int -> f a
  onStructureError :: forall a. String -> f a
  atKey :: forall a. String -> f a -> f a
  atIndex :: forall a. Int -> f a -> f a
  addTypeHint :: forall a. String -> f a -> f a
  addCtorHint :: forall a. String -> f a -> f a
  addSubtermHint :: forall a. Int -> f a -> f a
  addFieldHint :: forall a. Int -> f a -> f a
  altAccumulate :: forall a. f a -> f a -> f a
  altAccumulateLazy :: forall j a. (j -> f a) -> (j -> f a) -> j -> f a

-- altLast :: forall a. f a -> (Unit -> f a) -> f a

withKey :: forall f a. IsJsonDecoder f => String -> (Json -> f a) -> Json -> f a
withKey arg f j = atKey arg $ f j

withIndex :: forall f a. IsJsonDecoder f => Int -> (Json -> f a) -> Json -> f a
withIndex arg f j = atIndex arg $ f j

withTypeHint :: forall f a. IsJsonDecoder f => String -> (Json -> f a) -> Json -> f a
withTypeHint arg f j = addTypeHint arg $ f j

withCtorHint :: forall f a. IsJsonDecoder f => String -> (Json -> f a) -> Json -> f a
withCtorHint arg f j = addCtorHint arg $ f j

withSubtermHint :: forall f a. IsJsonDecoder f => Int -> (Json -> f a) -> Json -> f a
withSubtermHint arg f j = addSubtermHint arg $ f j

withFieldHint :: forall f a. IsJsonDecoder f => Int -> (Json -> f a) -> Json -> f a
withFieldHint arg f j = addFieldHint arg $ f j

withAttempts :: forall f a j b. IsJsonDecoder f => NonEmptyArray a -> (a -> j -> f b) -> j -> f b
withAttempts decoders fn j = (foldl1 (\a -> fn a) (\b a -> altAccumulateLazy b (fn a)) decoders) j

foldl1 :: forall a b. (a -> b) -> (b -> a -> b) -> NonEmptyArray a -> b
foldl1 f g neArr = go 1 (f $ NEA.head neArr)
  where
  len = NEA.length neArr
  go idx acc
    | idx < len = go (idx + 1) (g acc $ unsafePartial $ NEA.unsafeIndex neArr idx)
    | otherwise = acc

foldr1 :: forall a b. (a -> b) -> (a -> b -> b) -> NonEmptyArray a -> b
foldr1 f g neArr = go (NEA.length neArr - 2) (f $ NEA.last neArr)
  where
  go idx acc
    | idx > 0 = go (idx - 1) (g (unsafePartial $ NEA.unsafeIndex neArr idx) acc)
    | otherwise = acc
