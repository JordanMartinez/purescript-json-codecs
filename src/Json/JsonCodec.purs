module Json.JsonCodec where

import Prelude

import Codec.Codec (Codec, Codec', codec', decoder, encoder)
import Codec.Decoder (DecoderFn(..))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2)
import Data.List (List)
import Data.Tuple (Tuple, fst)
import Data.Validation.Semigroup (V(..), invalid)
import Foreign.Object (Object)
import Json.JsonDecoder (JsonDecoder)
import Json.JsonDecoder as JsonDecoder
import Json.Types (JsonErrorHandlers(..), JsonOffset)

-- | Overview of values:
-- | - e - the custom Json decoding error
-- | - extra - top-down custom data one may need for writing a codec. This is where
-- |           local overrides for typeclass instances can be provided.
-- |           If this value isn't needed, you should set this to `Unit`.
-- | - a - decode Json to a value of this type or encode it to Json
type JsonCodec e extra a = JsonCodec' e extra Json a
type JsonCodec' e extra from to = Codec' (Array JsonOffset) (JsonErrorHandlers e) e extra from to
type JIndexedCodec e extra a = Codec (Array JsonOffset) (JsonErrorHandlers e) e extra (Array Json) (List Json) a a
type JPropCodec e extra a = Codec (Array JsonOffset) (JsonErrorHandlers e) e extra (Object Json) (List (Tuple String Json)) a a

-- addOffset :: forall e extra a. JsonOffset -> Json -> JsonCodec e extra a -> JsonCodec e extra a
-- addOffset offset json (DecoderFn f) = DecoderFn $
--   mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra _ ->
--     runFn5 f (if h.includeJsonOffset then Array.snoc path offset else path) appendFn handlers extra json

-- onError :: forall e extra a. (Array JsonOffset -> e -> e) -> JsonCodec e extra a -> JsonCodec e extra a
-- onError mapErrs (DecoderFn f) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
--   lmap (mapErrs path) $ runFn5 f path appendFn handlers extra json

-- failWithMissingField :: forall e extra a. String -> JsonCodec e extra a
-- failWithMissingField str = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
--   invalid $ h.onMissingField path str

-- failWithMissingIndex :: forall e extra a. Int -> JsonCodec e extra a
-- failWithMissingIndex idx = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
--   invalid $ h.onMissingIndex path idx

-- failWithUnrefinableValue :: forall e extra a. String -> JsonCodec e extra a
-- failWithUnrefinableValue msg = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
--   invalid $ h.onUnrefinableValue path msg

-- failWithStructureError :: forall e extra a. String -> JsonCodec e extra a
-- failWithStructureError msg = DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ _ ->
--   invalid $ h.onStructureError path msg

-- addTypeHint :: forall e extra a. String -> JsonCodec e extra a -> JsonCodec e extra a
-- addTypeHint s = liftD (JsonDecoder.addTypeHint s)

-- addCtorHint :: forall e extra a. String -> JsonCodec e extra a -> JsonCodec e extra a
-- addCtorHint s = liftD (JsonDecoder.addCtorHint s)

-- addSubtermHint :: forall e extra a. Int -> JsonCodec e extra a -> JsonCodec e extra a
-- addSubtermHint i = liftD (JsonDecoder.addSubtermHint i)

-- addFieldHint :: forall e extra a. String -> JsonCodec e extra a -> JsonCodec e extra a
-- addFieldHint s = liftD (JsonDecoder.addFieldHint s)

-- -- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- -- | decodes using the second decoder. Errors from both decoders accumulate.
-- altAccumulate :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra a -> JsonCodec e extra a
-- altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
--   case unwrap $ runFn5 f1 path appendFn handlers extra json of
--     Left e -> case unwrap $ runFn5 f2 path appendFn handlers extra json of
--       Left e2 -> invalid $ appendFn e e2
--       Right a -> V $ Right a
--     Right a -> V $ Right a

-- -- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- -- | where one is decoding a sum type with a large number of data constructors.
-- altLast :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra a -> JsonCodec e extra a
-- altLast (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \path appendFn handlers extra json ->
--   case unwrap $ runFn5 f1 path appendFn handlers extra json of
--     Left _ -> runFn5 f2 path appendFn handlers extra json
--     Right a -> V $ Right a

mkJsonCodec
  ∷ ∀ e extra a
  . JsonDecoder e extra a
  → (a -> Json)
  → JsonCodec e extra a
mkJsonCodec dec enc = codec' dec $ mkFn2 \_ a -> enc a

decode
  :: forall e extra a
   . JsonErrorHandlers e
  -> (e -> e -> e)
  -> extra
  -> Json
  -> JsonCodec e extra a
  -> Either e a
decode handlers appendFn extra json codec =
  JsonDecoder.runJsonDecoder handlers appendFn extra json (decoder codec)

encode
  :: forall e extra a
   . extra
  -> a
  -> JsonCodec e extra a
  -> Json
encode extra a codec =
  fst $ runFn2 (encoder codec) extra a

refinedValue
  :: forall decodeError extra decodeFromEncodeTo decodeToEncodeFrom
   . (decodeFromEncodeTo -> Either String decodeToEncodeFrom)
  -> (decodeToEncodeFrom -> decodeFromEncodeTo)
  -> JsonCodec' decodeError extra decodeFromEncodeTo decodeToEncodeFrom
refinedValue refine unrefine =
  codec'
    ( DecoderFn $ mkFn5 \path _ (JsonErrorHandlers h) _ a ->
        case refine a of
          Left msg ->
            invalid $ h.onUnrefinableValue path msg
          Right b ->
            V $ Right b
    )
    (mkFn2 \_ a -> unrefine a)
