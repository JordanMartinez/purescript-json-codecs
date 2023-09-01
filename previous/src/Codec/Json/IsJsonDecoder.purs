module Codec.Json.IsJsonDecoder where

class IsJsonDecoder :: (Type -> Type) -> Constraint
class IsJsonDecoder f where
  onTypeMismatch :: forall a. String -> String -> f a
  onMissingField :: forall a. String -> f a
  onMissingIndex :: forall a. Int -> f a
  underKey :: forall a. String -> f a -> f a
  underIndex :: forall a. Int -> f a -> f a
  withTypeHint :: forall a. String -> f a -> f a
  withCtorHint :: forall a. String -> f a -> f a
  withSubtermHint :: forall a. Int -> f a -> f a
  withFieldHint :: forall a. Int -> f a -> f a
  altAccumulate :: forall a. f a -> f a -> f a
  altLast :: forall a. f a -> f a -> f a
