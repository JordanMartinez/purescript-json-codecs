module Test.Codec.Json.Unidirectional.PlainDodoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import Codec.Json.Errors.PlainDodoError (pdeD, printPlainDodoError, runJsonDecoderPDE)
import Codec.Json.JsonDecoder (JsonDecoder)
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeNumber, decodeString, decodeArray, decodeInt, decodeObject, decodeRecord)
import Codec.Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

runOutput :: Effect Unit
runOutput = do
  log "\n### PlainDodoError Output:"
  runDecoderPDE "Decode Int to Int" exampleInt decodeInt
  runDecoderPDE "Decode Int to String" exampleString decodeInt
  runDecoderPDE "Decode Record to Int" exampleRec decodeInt
  runDecoderPDE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPDE
  log "==="

runDecoderPDE
  :: forall a
   . Show a
  => String
  -> Json
  -> JsonDecoder (Doc Void) Unit a
  -> Effect Unit
runDecoderPDE msg example decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either printPlainDodoError show
    $ runJsonDecoderPDE example decoder

decodeRecIncorrectlyPDE :: JsonDecoder (Doc Void) Unit _
decodeRecIncorrectlyPDE =
  decodeRecord
    { boolean: pdeD decodeString
    , number: pdeD decodeBoolean
    , string: pdeD decodeNumber
    , int: pdeD decodeString
    , object: pdeD decodeInt
    , array: pdeD $ decodeObject decodeString
    , record: pdeD $ decodeArray decodeInt
    }

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
