module Codec.Json.Errors.NoError where

import Prelude

import Codec.Json.JsonDecoder (JsonDecoder, runJsonDecoder)
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
  , includeJsonOffset: false
  , addHint: mkFn3 \_ _ _ -> unit
  }

runJsonDecoderNone :: forall a. Json -> JsonDecoder Unit Unit a -> Maybe a
runJsonDecoderNone = runJsonDecoderNone' unit

runJsonDecoderNone' :: forall a extra. extra -> Json -> JsonDecoder Unit extra a -> Maybe a
runJsonDecoderNone' extra json = hush <<< runJsonDecoder handlersNone (\_ _ -> unit) extra json

noErr :: forall a. JsonDecoder Unit Unit a -> JsonDecoder Unit Unit a
noErr = identity

noErr' :: forall a extra. JsonDecoder Unit extra a -> JsonDecoder Unit extra a
noErr' = identity
