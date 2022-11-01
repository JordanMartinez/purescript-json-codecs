module Json.JsonDecoder.Qualified where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Data.Function.Uncurried (mkFn5, runFn5)
import Data.V.Semigroup.Qualified as V
import Json.JsonDecoder (JsonDecoder)

bind :: forall err extra a b. JsonDecoder err extra a -> (a -> JsonDecoder err extra b) -> JsonDecoder err extra b
bind (DecoderFn ma) f = DecoderFn $ mkFn5 \json pathSoFar appendFn handlers extra -> V.do
  a <- runFn5 ma json pathSoFar appendFn handlers extra
  let (DecoderFn mb) = f a
  runFn5 mb json pathSoFar appendFn handlers extra
