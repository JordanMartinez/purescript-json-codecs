module Codec.Json.JsonDecoder where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2, runFn3, runFn5)
import Data.Newtype (un, unwrap)
import Data.Validation.Semigroup (V(..), invalid)

-- | Same as `JsonDecoder'` but the `from` type variable is hard-coded to `Json`.
type JsonDecoder e extra a = DecoderFn (Array JsonOffset) (JsonErrorHandlers e) e extra Json a
type JsonDecoder' e extra from to = DecoderFn (Array JsonOffset) (JsonErrorHandlers e) e extra from to

addOffset :: forall e extra from a. JsonOffset -> Json -> JsonDecoder' e extra Json a -> JsonDecoder' e extra from a
addOffset offset json (DecoderFn f) = DecoderFn $
  mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra _ ->
    runFn5 f (runFn2 h.addJsonOffset path offset) appendFn handlers extra json

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

addTypeHintD :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addTypeHintD hint (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra json ->
  lmap (\x -> runFn3 h.addTypeHint path hint x) $ runFn5 f path appendFn handlers extra json

addCtorHintD :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addCtorHintD hint (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra json ->
  lmap (\x -> runFn3 h.addCtorHint path hint x) $ runFn5 f path appendFn handlers extra json

addSubtermHintD :: forall e extra from a. Int -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addSubtermHintD hint (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra json ->
  lmap (\x -> runFn3 h.addSubtermHint path hint x) $ runFn5 f path appendFn handlers extra json

addFieldHintD :: forall e extra from a. String -> JsonDecoder' e extra from a -> JsonDecoder' e extra from a
addFieldHintD hint (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra json ->
  lmap (\x -> runFn3 h.addFieldHint path hint x) $ runFn5 f path appendFn handlers extra json

-- | A function that determines how to accumulate errors (if any)
-- | - `altAccumulate`
-- | - `altLast`
type DecodeErrorAccumulatorFn e extra from to =
  JsonDecoder' e extra from to -> JsonDecoder' e extra from to -> JsonDecoder' e extra from to

-- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- | decodes using the second decoder. Errors from both decoders accumulate.
altAccumulate :: forall e extra from a. DecodeErrorAccumulatorFn e extra from a
altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
  case unwrap $ runFn5 f1 path appendFn handlers extra json of
    Left e -> case unwrap $ runFn5 f2 path appendFn handlers extra json of
      Left e2 -> invalid $ runFn2 appendFn e e2
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
  un V $ runFn5 fn [] (mkFn2 appendFn) handlers extra json
