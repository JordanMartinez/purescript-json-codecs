module Test.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dodo (plainText, twoSpaces)
import Dodo.Ansi (ansiGraphics)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Json.Decode.Class (decodeJson)
import Json.Encode.Class (encodeJson)
import Json.Errors.AnsiDodoError (AnsiDodoError, printAnsiDodoError)
import Json.Errors.AnsiDodoError as ADE
import Json.Errors.PlainDodoError (PlainDodoError, printPlainDodoError)
import Json.Errors.PlainDodoError as PDE
import Json.Errors.PrimitiveJsonError (PrimitiveJsonError, printPrimitiveJsonError)
import Json.Errors.PrimitiveJsonError as PJE
import Json.Primitive.Decode (class IsDecodeJsonError, JsonDecoder, runJsonDecoder)
import Json.Types (Optional(..))
import Json.Unidirectional.Encode.Value (encodeBoolean, encodeArray, encodeInt, encodeNumber, encodeObject, encodeRecord, encodeString, encodeUnitToNull)

main :: Effect Unit
main = do
  runDecoderPJE "Decode Int to Int" exampleInt PJE.decodeInt show
  runDecoderPJE "Decode Int to String" exampleString PJE.decodeInt show
  runDecoderPJE "Decode Record to Int" exampleRec PJE.decodeInt show
  runDecoderPJE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPJE show
  log "==="
  runDecoderPDE "Decode Int to Int" exampleInt PDE.decodeInt show
  runDecoderPDE "Decode Int to String" exampleString PDE.decodeInt show
  runDecoderPDE "Decode Record to Int" exampleRec PDE.decodeInt show
  runDecoderPDE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyPDE show
  log "==="
  runDecoderADE "Decode Int to Int" exampleInt ADE.decodeInt show
  runDecoderADE "Decode Int to String" exampleString ADE.decodeInt show
  runDecoderADE "Decode Record to Int" exampleRec ADE.decodeInt show
  runDecoderADE "Decode Record incorrectly" exampleRec decodeRecIncorrectlyADE show
  log "==="
  runDecoderADE "Decode Record incorrectly" exampleRec (decodeJson :: JsonDecoder AnsiDodoError IncorrectRecordType) show
  runDecoderADE "Decode Record incorrectly" exampleRec (decodeJson :: JsonDecoder AnsiDodoError { boolean :: Boolean, noExists :: Optional (Maybe Int) }) show
  runDecoderADE "Roundtrip check"
    ((encodeJson :: { noField :: Optional (Maybe Int) } -> Json) { noField: Optional Nothing })
    (decodeJson :: JsonDecoder AnsiDodoError { noField :: Optional (Maybe Int) })
    show

runDecoderPJE
  :: forall a
   . String
  -> Json
  -> JsonDecoder PrimitiveJsonError a
  -> (a -> String)
  -> Effect Unit
runDecoderPJE = runDecoder' printPrimitiveJsonError

runDecoderPDE
  :: forall a
   . String
  -> Json
  -> JsonDecoder PlainDodoError a
  -> (a -> String)
  -> Effect Unit
runDecoderPDE = runDecoder' (printPlainDodoError plainText twoSpaces)

runDecoderADE
  :: forall a
   . String
  -> Json
  -> JsonDecoder AnsiDodoError a
  -> (a -> String)
  -> Effect Unit
runDecoderADE = runDecoder' (printAnsiDodoError ansiGraphics twoSpaces)

runDecoder'
  :: forall err a
   . IsDecodeJsonError err
  => (err -> String)
  -> String
  -> Json
  -> JsonDecoder err a
  -> (a -> String)
  -> Effect Unit
runDecoder' printErr msg example decoder print =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either printErr print
    $ runJsonDecoder example decoder

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

decodeRecIncorrectlyPJE :: JsonDecoder PrimitiveJsonError _
decodeRecIncorrectlyPJE =
  PJE.decodeRecord
    { boolean: PJE.decodeString
    , number: PJE.decodeBoolean
    , string: PJE.decodeNumber
    , int: PJE.decodeString
    , object: PJE.decodeInt
    , array: PJE.decodeObject PJE.decodeString
    , record: PJE.decodeArray PJE.decodeInt
    }

decodeRecIncorrectlyPDE :: JsonDecoder PlainDodoError _
decodeRecIncorrectlyPDE =
  PDE.decodeRecord
    { boolean: PDE.decodeString
    , number: PDE.decodeBoolean
    , string: PDE.decodeNumber
    , int: PDE.decodeString
    , object: PDE.decodeInt
    , array: PDE.decodeObject PDE.decodeString
    , record: PDE.decodeArray PDE.decodeInt
    }

decodeRecIncorrectlyADE :: JsonDecoder AnsiDodoError _
decodeRecIncorrectlyADE =
  ADE.decodeRecord
    { boolean: ADE.decodeString
    , number: ADE.decodeBoolean
    , string: ADE.decodeNumber
    , int: ADE.decodeString
    , object: ADE.decodeInt
    , array: ADE.decodeObject ADE.decodeString
    , record: ADE.decodeArray ADE.decodeInt
    }
