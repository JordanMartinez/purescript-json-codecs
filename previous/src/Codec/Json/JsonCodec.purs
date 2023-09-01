module Codec.Json.JsonCodec where

import Prelude

import Codec.Codec (Codec, Codec', codec', decoder, encoder, mapDecodeError)
import Codec.Decoder (DecoderFn(..))
import Codec.Json.JsonDecoder (JsonDecoder, addCtorHintD, addFieldHintD, addSubtermHintD, addTypeHintD)
import Codec.Json.JsonDecoder as JsonDecoder
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2)
import Data.List (List)
import Data.Tuple (Tuple, fst)
import Data.Validation.Semigroup (V(..), invalid)
import Foreign.Object (Object)

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

addTypeHintC
  :: forall e extra a b c d
   . String
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
addTypeHintC hint = mapDecodeError (addTypeHintD hint)

addCtorHintC
  :: forall e extra a b c d
   . String
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
addCtorHintC hint = mapDecodeError (addCtorHintD hint)

addSubtermHintC
  :: forall e extra a b c d
   . Int
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
addSubtermHintC hint = mapDecodeError (addSubtermHintD hint)

addFieldHintC
  :: forall e extra a b c d
   . String
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
  -> Codec (Array JsonOffset) (JsonErrorHandlers e) e extra a b c d
addFieldHintC hint = mapDecodeError (addFieldHintD hint)

-- | ```
-- | import Data.String.NonEmpty as NES
-- |
-- | string >~> refinedValue (note "Received empty string" <<< NES.fromString) (NES.toString)
-- | ```
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
            invalid $ runFn2 h.onUnrefinableValue path msg
          Right b ->
            V $ Right b
    )
    (mkFn2 \_ a -> unrefine a)
