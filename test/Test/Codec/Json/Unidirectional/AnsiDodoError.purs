module Test.Json.Unidirectional.AnsiDodoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo.Ansi (GraphicsParam)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import Codec.Json.Errors.AnsiDodoError (ade, printAnsiDodoError, runJsonDecoderADE)
import Codec.Json.JsonDecoder (JsonDecoder)
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeNumber, decodeString, decodeArray, decodeInt, decodeObject, decodeRecord)
import Codec.Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

runOutput :: Effect Unit
runOutput = do
  log "\n### AnsiDodoError Output:"
  runDecoderADE "Decode Int to Int" exampleInt decodeInt
  runDecoderADE "Decode Int to String" exampleString decodeInt
  runDecoderADE "Decode Record to Int" exampleRec decodeInt
  runDecoderADE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyADE
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

exampleInt :: Json
exampleInt = encodeInt 1

exampleString :: Json
exampleString = encodeString "foo"

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

decodeRecIncorrectlyADE :: JsonDecoder (Doc GraphicsParam) Unit _
decodeRecIncorrectlyADE =
  decodeRecord
    { boolean: ade decodeString
    , number: ade decodeBoolean
    , string: ade decodeNumber
    , int: ade decodeString
    , object: ade decodeInt
    , array: ade $ decodeObject decodeString
    , record: ade $ decodeArray decodeInt
    }
