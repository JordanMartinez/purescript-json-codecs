module Codec.Decoder where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, mkFn5, runFn5)
import Data.Newtype (class Newtype, unwrap)
import Data.Validation.Semigroup (V(..), invalid)

-- Think of this as `ReaderT from m to`
newtype DecoderFn from path handlers e extra to = DecoderFn (Fn5 from path (e -> e -> e) handlers extra (V e to))

derive instance Newtype (DecoderFn from path handlers e extra to) _

instance functorDecoderFn :: Functor (DecoderFn from path handlers e extra) where
  map f (DecoderFn fn) = DecoderFn $ mkFn5 \from path appendFn handlers extra ->
    f <$> runFn5 fn from path appendFn handlers extra

instance applyDecoderFn :: Apply (DecoderFn from path handlers e extra) where
  apply (DecoderFn ff) (DecoderFn fa) = DecoderFn $ mkFn5 \from path appendFn handlers extra ->
    case runFn5 ff from path appendFn handlers extra, runFn5 fa from path appendFn handlers extra of
      V (Left e1), V (Left e2) -> V (Left $ appendFn e1 e2)
      V (Left e1), _ -> V (Left e1)
      _, V (Left e2) -> V (Left e2)
      V (Right f'), V (Right a') -> V (Right (f' a'))

instance applicativeDecoderFn :: Applicative (DecoderFn from path handlers e extra) where
  pure a = DecoderFn $ mkFn5 \_ _ _ _ _ -> V $ Right a

getPath :: forall from path handlers e extra. DecoderFn from path handlers e extra path
getPath = DecoderFn $ mkFn5 \_ path _ _ _ -> V $ Right path

-- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- | decodes using the second decoder. Errors from both decoders accumulate.
altAccumulate :: forall from path handlers e extra a. DecoderFn from path handlers e extra a -> DecoderFn from path handlers e extra a -> DecoderFn from path handlers e extra a
altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \from path appendFn handlers extra ->
  case unwrap $ runFn5 f1 from path appendFn handlers extra of
    Left e -> case unwrap $ runFn5 f2 from path appendFn handlers extra of
      Left e2 -> invalid $ appendFn e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

-- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- | where one is decoding a sum type with a large number of data constructors.
altLast :: forall from path handlers e extra a. DecoderFn from path handlers e extra a -> DecoderFn from path handlers e extra a -> DecoderFn from path handlers e extra a
altLast (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \from path appendFn handlers extra ->
  case unwrap $ runFn5 f1 from path appendFn handlers extra of
    Left _ -> runFn5 f2 from path appendFn handlers extra
    Right a -> V $ Right a
