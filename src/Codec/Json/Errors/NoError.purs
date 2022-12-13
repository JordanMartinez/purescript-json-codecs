module Codec.Json.Errors.NoError where

import Prelude

import Codec.Json.JsonCodec (JsonCodec', JsonCodec, decode)
import Codec.Json.JsonDecoder (JsonDecoder, JsonDecoder', runJsonDecoder)
import Codec.Json.Types (JsonErrorHandlers(..))
import Data.Argonaut.Core (Json)
import Data.Either (hush)
import Data.Function.Uncurried (mkFn2, mkFn3)
import Data.Maybe (Maybe)

handlersNone :: JsonErrorHandlers Unit
handlersNone = JsonErrorHandlers
  { onTypeMismatch: mkFn3 \_ _ _ -> unit
  , onMissingField: mkFn2 \_ _ -> unit
  , onMissingIndex: mkFn2 \_ _ -> unit
  , onUnrefinableValue: mkFn2 \_ _ -> unit
  , onStructureError: mkFn2 \_ _ -> unit
  , addJsonOffset: mkFn2 \a _ -> a
  , addTypeHint: mkFn3 \_ _ _ -> unit
  , addCtorHint: mkFn3 \_ _ _ -> unit
  , addSubtermHint: mkFn3 \_ _ _ -> unit
  , addFieldHint: mkFn3 \_ _ _ -> unit
  }

runJsonDecoderNoErrs :: forall a. Json -> JsonDecoder Unit Unit a -> Maybe a
runJsonDecoderNoErrs = runJsonDecoderNoErrs' unit

runJsonDecoderNoErrs' :: forall a extra. extra -> Json -> JsonDecoder Unit extra a -> Maybe a
runJsonDecoderNoErrs' extra json = hush <<< runJsonDecoder handlersNone (\_ _ -> unit) extra json

decodeNoErrs :: forall a. Json -> JsonCodec Unit Unit a -> Maybe a
decodeNoErrs = decodeNoErrs' unit

decodeNoErrs' :: forall a extra. extra -> Json -> JsonCodec Unit extra a -> Maybe a
decodeNoErrs' extra json = hush <<< decode handlersNone (\_ _ -> unit) extra json

noErrD :: forall from to. JsonDecoder' Unit Unit from to -> JsonDecoder' Unit Unit from to
noErrD = identity

noErrC :: forall from to. JsonCodec' Unit Unit from to -> JsonCodec' Unit Unit from to
noErrC = identity
