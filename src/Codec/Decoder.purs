module Codec.Decoder where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, mkFn5, runFn5)
import Data.Newtype (class Newtype, unwrap)
import Data.Validation.Semigroup (V(..), invalid)

-- | Think of this as
-- | ```
-- | forall e
-- |   . Semigroup e
-- |  => { path :: path         -- current 'path' in `from` value (e.g. Json)
-- |     , handlers :: handlers -- handlers for building custom error
-- |     , extra :: extra       -- a slot for any extra goodies (e.g. "local" type class instances)
-- |     }
-- |  -> from                   -- value being decoded
-- |  -> V e to                 -- monad that 
-- |                            --  1. fails with accumulated error using `<>`, or
-- |                            --  2. succeeds with `to` value
-- | ```
-- | but where 
-- | - the `from -> V e to` function has been converted to an uncurried function: `Fn1 from (V e to)`
-- | - each of the record's labels has been spread into "slots" in an uncurried function
-- |    `Fn1 from (V e to)` -> `Fn4 path handler extra from (V e to)`
-- | - and the `Semigroup`'s `append`/`<>` has been inlined to reduce type class dictionary overhead
-- |    `Fn4 path handler extra from (V e to)` -> `Fn5 path appendFn handler extra from (V e to)`
newtype DecoderFn path handlers e extra from to = DecoderFn (Fn5 path (e -> e -> e) handlers extra from (V e to))

derive instance Newtype (DecoderFn path handlers e extra from to) _

instance functorDecoderFn :: Functor (DecoderFn path handlers e extra from) where
  map f (DecoderFn fn) = DecoderFn $ mkFn5 \path appendFn handlers extra from ->
    f <$> runFn5 fn path appendFn handlers extra from

instance applyDecoderFn :: Apply (DecoderFn path handlers e extra from) where
  apply (DecoderFn ff) (DecoderFn fa) = DecoderFn $ mkFn5 \path appendFn handlers extra from ->
    case runFn5 ff path appendFn handlers extra from, runFn5 fa path appendFn handlers extra from of
      V (Left e1), V (Left e2) -> V (Left $ appendFn e1 e2)
      V (Left e1), _ -> V (Left e1)
      _, V (Left e2) -> V (Left e2)
      V (Right f'), V (Right a') -> V (Right (f' a'))

instance applicativeDecoderFn :: Applicative (DecoderFn path handlers e extra from) where
  pure a = DecoderFn $ mkFn5 \_ _ _ _ _ -> V $ Right a

getPath :: forall path handlers e extra from. DecoderFn path handlers e extra from path
getPath = DecoderFn $ mkFn5 \path _ _ _ _ -> V $ Right path

-- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- | decodes using the second decoder. Errors from both decoders accumulate.
altAccumulate :: forall path handlers e extra from a. DecoderFn path handlers e extra from a -> DecoderFn path handlers e extra from a -> DecoderFn path handlers e extra from a
altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra from ->
  case unwrap $ runFn5 f1 path appendFn handlers extra from of
    Left e -> case unwrap $ runFn5 f2 path appendFn handlers extra from of
      Left e2 -> invalid $ appendFn e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

-- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- | where one is decoding a sum type with a large number of data constructors.
altLast :: forall path handlers e extra from a. DecoderFn path handlers e extra from a -> DecoderFn path handlers e extra from a -> DecoderFn path handlers e extra from a
altLast (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra from ->
  case unwrap $ runFn5 f1 path appendFn handlers extra from of
    Left _ -> runFn5 f2 path appendFn handlers extra from
    Right a -> V $ Right a
