module Test.Codec.Json.Unidirectional.Value where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError, fromArray, fromBoolean, fromInt, fromNumber, fromObject, fromOption, fromOptionArray, fromOptionRename, fromRecord, fromRequired, fromRequired', fromRequiredRename, fromString, fromUnit, printDecodeError, toArray, toBoolean, toInt, toNumber, toObject, toRecord, toRequired, toString)
import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object

runOutput :: Effect Unit
runOutput = do
  log "\n### PrimitiveJsonError Output:"
  runDecoder "Decode Int to Int" exampleInt toInt
  runDecoder "Decode Int to String" exampleString toInt
  runDecoder "Decode Record to Int" exampleRec toInt
  runDecoder "Decode Record incorrectly" exampleRec toRecIncorrectly
  log "==="
  log "Encoding record"
  log $ stringifyWithIndent 2 $ encodedRecord

runDecoder
  :: forall a
   . Show a
  => String
  -> Json
  -> (Json -> Either DecodeError a)
  -> Effect Unit
runDecoder msg example decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either printDecodeError show
    $ decoder example

toRecIncorrectly
  :: Json
  -> Either DecodeError
       { array :: Object String
       , boolean :: String
       , int :: String
       , number :: Boolean
       , object :: Int
       , record :: Array Int
       , string :: Number
       }

toRecIncorrectly =
  toRecord
    { boolean: toRequired toString
    , number: toRequired toBoolean
    , string: toRequired toNumber
    , int: toRequired toString
    , object: toRequired toInt
    , array: toRequired $ toObject toString
    , record: toRequired $ toArray toInt
    }

exampleInt :: Json
exampleInt = fromInt 1

exampleString :: Json
exampleString = fromString "foo"

exampleRec :: Json
exampleRec = exampleRecValue #
  fromRecord
    { boolean: fromRequired fromBoolean
    , number: fromRequired fromNumber
    , string: fromRequired fromString
    , int: fromRequired fromInt
    , object: fromRequired $ fromObject fromString
    , array: fromRequired $ fromArray fromInt
    , record: fromRequired $ fromRecord
        { foo: fromRequired fromString
        , bar: fromRequired fromUnit
        }
    }
  where
  exampleRecValue =
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

encodedRecord :: Json
encodedRecord = fromRecord
  { req: fromRequired fromInt
  , reqRen: fromRequiredRename "otherName" fromString
  , zAppearsFirst: fromRequired' @1 fromString
  , opt: fromOption fromString
  , optRen: fromOptionRename "otherName2" fromString
  , optArr: fromOptionArray fromString
  , nested: fromRequired $ fromRecord
      { other: fromRequired fromBoolean
      , foo: fromOption fromBoolean
      }
  }
  { req: 1
  , reqRen: "two"
  , zAppearsFirst: "three"
  , opt: Nothing
  , optRen: Just "hello"
  , optArr: []
  , nested:
      { other: true
      , foo: Just false
      }
  }
