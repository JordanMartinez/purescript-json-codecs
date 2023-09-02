module Test.Codec.Json.Unidirectional.Value where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Codec.Json.Unidirectional.Decode.Value (decodeArray, decodeBoolean, decodeInt, decodeNumber, decodeObject, decodeString, toRecord, toRequired)
import Codec.Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object

runOutput :: forall @f. IsJsonDecoder f => (forall a. Show a => f a -> String) -> Effect Unit
runOutput printResult = do
  log "\n### PrimitiveJsonError Output:"
  runDecoder @f "Decode Int to Int" exampleInt printResult decodeInt
  runDecoder @f "Decode Int to String" exampleString printResult decodeInt
  runDecoder @f "Decode Record to Int" exampleRec printResult decodeInt
  runDecoder @f "Decode Record incorrectly" exampleRec printResult decodeRecIncorrectly
  log "==="

runDecoder
  :: forall @f a
   . String
  -> Json
  -> (f a -> String)
  -> (Json -> f a)
  -> Effect Unit
runDecoder msg example printer decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ printer
    $ decoder example

decodeRecIncorrectly :: forall @f. IsJsonDecoder f => Json -> f _
decodeRecIncorrectly =
  toRecord
    { boolean: toRequired @f decodeString
    , number: toRequired @f decodeBoolean
    , string: toRequired @f decodeNumber
    , int: toRequired @f decodeString
    , object: toRequired @f decodeInt
    , array: toRequired @f $ decodeObject decodeString
    , record: toRequired @f $ decodeArray decodeInt
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

