module Test.Codec.Json.Unidirectional.Value where

import Prelude

import Codec.Json.Unidirectional.Value (toArray, toBoolean, toInt, toNumber, toObject, toString, toRecord, toRequired, fromArray, fromBoolean, fromInt, fromNumber, fromObject, fromRecord, fromRequired, fromString, fromUnit)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Foldable as Foldable
import Data.List (List)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object

runOutput :: Effect Unit
runOutput = do
  log "\n### PrimitiveJsonError Output:"
  runDecoder "Decode Int to Int" exampleInt toInt
  runDecoder "Decode Int to String" exampleString toInt
  runDecoder "Decode Record to Int" exampleRec toInt
  runDecoder "Decode Record incorrectly" exampleRec toRecIncorrectly
  log "==="

runDecoder
  :: forall a
   . Show a
  => String
  -> Json
  -> (Json -> Either (List String) a)
  -> Effect Unit
runDecoder msg example decoder =
  log
    $ append ("\n" <> msg <> ":\n")
    $ either Foldable.fold show
    $ decoder example

toRecIncorrectly :: Json -> Either (List String) _
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
exampleRec =
  fromRecord fromr value
  where
  fromr =
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

