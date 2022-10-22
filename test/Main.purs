module Test.Main where

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
import Json.Decode.Class (decodeJson)
import Json.Encode.Class (encodeJson)
import Json.Errors.AnsiDodoError (ade, printAnsiDodoError, runJsonDecoderADE)
import Json.Errors.PlainDodoError (pde, printPlainDodoError, runJsonDecoderPDE)
import Json.Errors.PrimitiveJsonError (PrimitiveJsonError, pje, printPrimitiveJsonError, runJsonDecoderPJE)
import Json.Primitive.Decode (JsonDecoder, decodeBoolean, decodeNumber, decodeString)
import Json.Types (Optional(..))
import Json.Unidirectional.Decode.Value (decodeArray, decodeInt, decodeObject, decodeRecord)
import Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

main :: Effect Unit
main = do
  runDecoderPJE "Decode Int to Int" exampleInt decodeInt
  runDecoderPJE "Decode Int to String" exampleString decodeInt
  runDecoderPJE "Decode Record to Int" exampleRec decodeInt
  runDecoderPJE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPJE
  log "==="
  runDecoderPDE "Decode Int to Int" exampleInt decodeInt
  runDecoderPDE "Decode Int to String" exampleString decodeInt
  runDecoderPDE "Decode Record to Int" exampleRec decodeInt
  runDecoderPDE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPDE
  log "==="
  runDecoderADE "Decode Int to Int" exampleInt decodeInt
  runDecoderADE "Decode Int to String" exampleString decodeInt
  runDecoderADE "Decode Record to Int" exampleRec decodeInt
  runDecoderADE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyADE
  log "==="
  runDecoderADE "Decode Record incorrectly" exampleRec (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit IncorrectRecordType)
  runDecoderADE "Decode Record incorrectly" exampleRec (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit { boolean :: Boolean, noExists :: Optional (Maybe Int) })
  runDecoderADE "Roundtrip check"
    ((encodeJson :: { noField :: Optional (Maybe Int) } -> Json) { noField: Optional Nothing })
    (decodeJson :: JsonDecoder (Doc GraphicsParam) Unit { noField :: Optional (Maybe Int) })

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

type IncorrectRecordType =
  { boolean :: String
  , number :: Boolean
  , string :: Number
  , int :: String
  , object :: Int
  , array :: Object String
  , record :: Array Int
  }

decodeRecIncorrectlyPJE :: JsonDecoder PrimitiveJsonError Unit _
decodeRecIncorrectlyPJE =
  decodeRecord
    { boolean: pje decodeString
    , number: pje decodeBoolean
    , string: pje decodeNumber
    , int: pje decodeString
    , object: pje decodeInt
    , array: pje $ decodeObject decodeString
    , record: pje $ decodeArray decodeInt
    }

decodeRecIncorrectlyPDE :: JsonDecoder (Doc Void) Unit _
decodeRecIncorrectlyPDE =
  decodeRecord
    { boolean: pde decodeString
    , number: pde decodeBoolean
    , string: pde decodeNumber
    , int: pde decodeString
    , object: pde decodeInt
    , array: pde $ decodeObject decodeString
    , record: pde $ decodeArray decodeInt
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
