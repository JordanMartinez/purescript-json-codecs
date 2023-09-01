-- | Defines the handlers and other utilities for when you do not want
-- | any error when a JSON decoder fails.
module Codec.Json.Errors.NoError where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))

newtype SpeedyDecoder a = SpeedyDecoder (Maybe a)

instance IsJsonDecoder SpeedyDecoder where
  onTypeMismatch :: forall a. String -> String -> SpeedyDecoder a
  onTypeMismatch _ _ = SpeedyDecoder Nothing

  onMissingField :: forall a. String -> SpeedyDecoder a
  onMissingField _ = SpeedyDecoder Nothing

  onMissingIndex :: forall a. Int -> SpeedyDecoder a
  onMissingIndex _ = SpeedyDecoder Nothing

  underKey :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  underKey _ = identity

  underIndex :: forall a. Int -> SpeedyDecoder a -> SpeedyDecoder a
  underIndex _ = identity

  withTypeHint :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  withTypeHint _ = identity

  withCtorHint :: forall a. String -> SpeedyDecoder a -> SpeedyDecoder a
  withCtorHint _ = identity

  withSubtermHint :: forall a. Int -> SpeedyDecoder a -> SpeedyDecoder a
  withSubtermHint _ = identity

  withFieldHint :: forall a. Int -> SpeedyDecoder a -> SpeedyDecoder a
  withFieldHint _ = identity

  altAccumulate :: forall a. SpeedyDecoder a -> SpeedyDecoder a -> SpeedyDecoder a
  altAccumulate (SpeedyDecoder l) (SpeedyDecoder r) = SpeedyDecoder $ l <|> r

  altLast :: forall a. SpeedyDecoder a -> SpeedyDecoder a -> SpeedyDecoder a
  altLast (SpeedyDecoder l) (SpeedyDecoder r) = SpeedyDecoder $ l <|> r

