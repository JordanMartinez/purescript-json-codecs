module Json.Errors.NoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Json.JsonDecoder (JsonDecoder, JsonErrorHandlers, runJsonDecoder)

handlersNone :: JsonErrorHandlers Unit
handlersNone =
  { append: \_ _ -> unit
  , onTypeMismatch: \_ _ _ -> unit
  , onMissingField: \_ _ -> unit
  , onMissingIndex: \_ _ -> unit
  , onUnrefinableValue: \_ _ -> unit
  , onStructureError: \_ _ -> unit
  , includeJsonOffset: false
  , addHint: \_ _ _ -> unit
  }

runJsonDecoderNone :: forall a. Json -> JsonDecoder Unit Unit a -> Maybe a
runJsonDecoderNone = runJsonDecoderNone' unit

runJsonDecoderNone' :: forall a extra. extra -> Json -> JsonDecoder Unit extra a -> Maybe a
runJsonDecoderNone' extra json = hush <<< runJsonDecoder handlersNone extra json

noErr :: forall a. JsonDecoder Unit Unit a -> JsonDecoder Unit Unit a
noErr = identity

noErr' :: forall a extra. JsonDecoder Unit extra a -> JsonDecoder Unit extra a
noErr' = identity
