-- | Defines the handlers and other utilities for when you do not want
-- | any error when a JSON decoder fails.
module Codec.Json.Decoders.SpeedyDecoder where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Control.Alt ((<|>))
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)

newtype SpeedyDecoder a = SpeedyDecoder (Maybe a)

derive instance Eq a => Eq (SpeedyDecoder a)
derive instance Ord a => Ord (SpeedyDecoder a)
derive instance Newtype (SpeedyDecoder a) _
derive instance Generic (SpeedyDecoder a) _
instance Show a => Show (SpeedyDecoder a) where
  show x = genericShow x

derive newtype instance Functor SpeedyDecoder
derive newtype instance Apply SpeedyDecoder
derive newtype instance Applicative SpeedyDecoder
derive newtype instance Bind SpeedyDecoder
derive newtype instance Monad SpeedyDecoder
derive instance Foldable SpeedyDecoder
derive instance Traversable SpeedyDecoder

instance IsJsonDecoder SpeedyDecoder where
  onUnrefinableValue :: forall a. String -> SpeedyDecoder a
  onUnrefinableValue _ = SpeedyDecoder Nothing

  onTypeMismatch :: forall a. Fn2 String String (SpeedyDecoder a)
  onTypeMismatch = mkFn2 \_ _ -> SpeedyDecoder Nothing

  onMissingField :: forall a. String -> SpeedyDecoder a
  onMissingField _ = SpeedyDecoder Nothing

  onMissingIndex :: forall a. Int -> SpeedyDecoder a
  onMissingIndex _ = SpeedyDecoder Nothing

  onStructureError :: forall a. String -> SpeedyDecoder a
  onStructureError _ = SpeedyDecoder Nothing

  atKey :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  atKey _ = identity

  atIndex :: forall a. Int -> SpeedyDecoder a -> SpeedyDecoder a
  atIndex _ = identity

  addTypeHint :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  addTypeHint _ = identity

  addCtorHint :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  addCtorHint _ = identity

  addSubtermHint :: forall a. Int -> SpeedyDecoder a -> SpeedyDecoder a
  addSubtermHint _ = identity

  addFieldHint :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  addFieldHint _ = identity

  altAccumulate :: forall a. SpeedyDecoder a -> SpeedyDecoder a -> SpeedyDecoder a
  altAccumulate (SpeedyDecoder l) (SpeedyDecoder r) = SpeedyDecoder $ l <|> r

  altAccumulateLazy :: forall j a. (j -> SpeedyDecoder a) -> (j -> SpeedyDecoder a) -> j -> SpeedyDecoder a
  altAccumulateLazy l r j = case l j of
    x@(SpeedyDecoder (Just _)) -> x
    _ -> r j

runSpeedyDecoder :: forall a. SpeedyDecoder a -> Maybe a
runSpeedyDecoder (SpeedyDecoder a) = a
