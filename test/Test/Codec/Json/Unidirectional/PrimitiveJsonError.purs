module Test.Codec.Json.Unidirectional.PrimitiveJsonError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import Codec.Json.Errors.PrimitiveJsonError (PrimitiveJsonError, pjeD, printPrimitiveJsonError, runJsonDecoderPJE)
import Codec.Json.JsonDecoder (JsonDecoder)
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeNumber, decodeString, decodeArray, decodeInt, decodeObject, decodeRecord)
import Codec.Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

runOutput :: Effect Unit
runOutput = do
  log "\n### PrimitiveJsonError Output:"
  runDecoderPJE "Decode Int to Int" exampleInt decodeInt
  runDecoderPJE "Decode Int to String" exampleString decodeInt
  runDecoderPJE "Decode Record to Int" exampleRec decodeInt
  runDecoderPJE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPJE
  log "==="

runDecoderPJE
  :: forall a
   . Show a
  => String
  -> Json
  -> JsonDecoder PrimitiveJsonError Unit a
  -> Effect Unit
runDecoderPJE msg example decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either printPrimitiveJsonError show
    $ runJsonDecoderPJE example decoder

decodeRecIncorrectlyPJE :: JsonDecoder PrimitiveJsonError Unit _
decodeRecIncorrectlyPJE =
  decodeRecord
    { boolean: pjeD decodeString
    , number: pjeD decodeBoolean
    , string: pjeD decodeNumber
    , int: pjeD decodeString
    , object: pjeD decodeInt
    , array: pjeD $ decodeObject decodeString
    , record: pjeD $ decodeArray decodeInt
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

