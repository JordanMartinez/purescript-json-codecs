module Codec.Json.JsonDecoder where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset, TypeHint(..))
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn5, runFn2, runFn3, runFn5)
import Data.Newtype (un, unwrap)
import Data.Validation.Semigroup (V(..), invalid)

-- | Overview of values:
-- | - Json - the JSON value currently being decoded at this point
-- | - Array JsonOffset - the position within the larger JSON that the current JSON is located
-- | - JsonErrorHandlers e - runtime-configured way to handling errors
-- | - extra - top-down custom data one may need for writing a decoder. This is where
-- |           local overrides for typeclass instances can be provided.
-- |           If this value isn't needed, you should set this to `Unit`.
-- | - V e a - an Either-like monad that accumulates errors using the `append` function in the handlers arg.
type JsonDecoder e extra a = DecoderFn (Array JsonOffset) (JsonErrorHandlers e) e extra Json a
type JsonDecoder' e extra from to = DecoderFn (Array JsonOffset) (JsonErrorHandlers e) e extra from to

addOffset :: forall e extra from a. JsonOffset -> Json -> JsonDecoder' e extra Json a -> JsonDecoder' e extra from a
addOffset offset json (DecoderFn f) = DecoderFn $
  mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra _ ->
    runFn5 f (if h.includeJsonOffset then Array.snoc path offset else path) appendFn handlers extra json

onError :: forall e extra from a. (Array JsonOffset -> e -> e) -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
onError mapErrs (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
  lmap (mapErrs path) $ runFn5 f path appendFn handlers extra json

failWithMissingField :: forall e extra from a. String -> JsonDecoder' e extra from a
failWithMissingField str = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
  invalid $ runFn2 h.onMissingField path str

failWithMissingIndex :: forall e extra from a. Int -> JsonDecoder' e extra from a
failWithMissingIndex idx = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
  invalid $ runFn2 h.onMissingIndex path idx

failWithUnrefinableValue :: forall e extra from a. String -> JsonDecoder' e extra from a
failWithUnrefinableValue msg = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
  invalid $ runFn2 h.onUnrefinableValue path msg

failWithStructureError :: forall e extra from a. String -> JsonDecoder' e extra from a
failWithStructureError msg = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
  invalid $ runFn2 h.onStructureError path msg

addHint :: forall e extra from a. TypeHint -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addHint hint (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra json ->
  lmap (\x -> runFn3 h.addHint path hint x) $ runFn5 f path appendFn handlers extra json

addTypeHint :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addTypeHint = addHint <<< TyName

addCtorHint :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addCtorHint = addHint <<< CtorName

addSubtermHint :: forall e extra from a. Int -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addSubtermHint = addHint <<< Subterm

addFieldHint :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addFieldHint = addHint <<< Field

type DecodeErrorAccumulatorFn e extra from to =
  JsonDecoder' e extra from to -> JsonDecoder' e extra from to -> JsonDecoder' e extra from to

-- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- | decodes using the second decoder. Errors from both decoders accumulate.
altAccumulate :: forall e extra from a. DecodeErrorAccumulatorFn e extra from a
altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
  case unwrap $ runFn5 f1 path appendFn handlers extra json of
    Left e -> case unwrap $ runFn5 f2 path appendFn handlers extra json of
      Left e2 -> invalid $ appendFn e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

-- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- | where one is decoding a sum type with a large number of data constructors.
altLast :: forall e extra from a. DecodeErrorAccumulatorFn e extra from a
altLast (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
  case unwrap $ runFn5 f1 path appendFn handlers extra json of
    Left _ -> runFn5 f2 path appendFn handlers extra json
    Right a -> V $ Right a

runJsonDecoder
  :: forall e extra a
   . JsonErrorHandlers e
  -> (e -> e -> e)
  -> extra
  -> Json
  -> JsonDecoder e extra a
  -> Either e a
runJsonDecoder handlers appendFn extra json (DecoderFn fn) =
  un V $ runFn5 fn [] appendFn handlers extra json
