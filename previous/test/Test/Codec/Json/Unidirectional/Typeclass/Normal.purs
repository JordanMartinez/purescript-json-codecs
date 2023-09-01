module Test.Codec.Json.Unidirectional.Typeclass.Normal where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo.Ansi (GraphicsParam)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Codec.Json.Errors.AnsiDodoError (printAnsiDodoError, runJsonDecoderADE)
import Codec.Json.JsonDecoder (JsonDecoder)
import Codec.Json.Newtypes (Optional(..))
import Codec.Json.Unidirectional.Decode.Class (decodeJson)
import Codec.Json.Unidirectional.Encode.Class (encodeJson)
import Codec.Json.Unidirectional.Encode.Value (encodeArray, encodeBoolean, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

runOutput :: Effect Unit
runOutput = do
  log "\n### Typeclass (Normal) Output:"
  runDecoderADE "Decode Record incorrectly" exampleRec
    (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit IncorrectRecordType)
  runDecoderADE "Decode Record incorrectly (test optional field)" exampleRec
    (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit { boolean :: Boolean, noExists :: Optional (Maybe Int) })
  runDecoderADE "Roundtrip check"
    ( (encodeJson :: { noField :: Optional (Maybe Int) } -> Json)
        { noField: Optional Nothing }
    )
    (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit { noField :: Optional (Maybe Int) })
  log "==="

runDecoderADE
  :: forall a
   . Show a
  => String
  -> Json
  -> JsonDecoder (Doc GraphicsParam) Unit a
  -> Effect Unit
runDecoderADE msg example decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either printAnsiDodoError show
    $ runJsonDecoderADE example decoder

exampleRec :: Json
exampleRec =
  encodeRecord encoder value
  where
  encoder =
    { boolean: encodeBoolean
    , number: encodeNumber
    , string: encodeString
    , int: encodeInt
    , object: encodeObject encodeString
    , array: encodeArray encodeInt
    , record: encodeRecord
        { foo: encodeString
        , bar: encodeUnitToNull
        }
    }
  value =
    { boolean: true
    , number: 1.4
    , string: "hello"
    , int: 9
    , object: Object.fromFoldable
        [ Tuple "a" "1"
        , Tuple "b" "2"
        , Tuple "c" "3"
        , Tuple "d" "4"
        ]
    , array: Array.range 1 10
    , record:
        { foo: "what"
        , bar: unit
        }
    }

type IncorrectRecordType =
  { boolean :: String
  , number :: Boolean
  , string :: Number
  , int :: String
  , object :: Int
  , array :: Object String
  , record :: Array Int
  }
