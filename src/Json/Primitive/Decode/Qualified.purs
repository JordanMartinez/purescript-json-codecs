module Json.Primitive.Decode.Qualified where

import Prelude

import Data.Function.Uncurried (mkFn4, runFn4)
import Data.V.Semigroup.Qualified as V
import Json.Primitive.Decode (JsonDecoder(..))

bind :: forall err extra a b. JsonDecoder err extra a -> (a -> JsonDecoder err extra b) -> JsonDecoder err extra b
bind (JsonDecoder ma) f = JsonDecoder $ mkFn4 \json pathSoFar handlers extra -> V.do
  a <- runFn4 ma json pathSoFar handlers extra
  let (JsonDecoder mb) = f a
  runFn4 mb json pathSoFar handlers extra
