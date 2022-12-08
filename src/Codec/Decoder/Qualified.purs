module Codec.Decoder.Qualified where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Data.Function.Uncurried (mkFn5, runFn5)
import Data.V.Semigroup.Qualified as V

bind
  :: forall path handlers e extra from a b
   . DecoderFn path handlers e extra from a
  -> (a -> DecoderFn path handlers e extra from b)
  -> DecoderFn path handlers e extra from b
bind (DecoderFn ma) f = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra a -> V.do
  a' <- runFn5 ma pathSoFar appendFn handlers extra a
  let (DecoderFn mb) = f a'
  runFn5 mb pathSoFar appendFn handlers extra a
