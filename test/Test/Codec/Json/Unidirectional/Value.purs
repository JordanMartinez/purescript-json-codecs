module Test.Codec.Json.Unidirectional.Value where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Codec.Json.Unidirectional.Value (toArray, toBoolean, toInt, toNumber, toObject, toString, toRecord, toRequired, fromArray, fromBoolean, fromInt, fromNumber, fromObject, fromRecord, fromRequired, fromString, fromUnitToNull)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object

runOutput :: forall @f. IsJsonDecoder f => (forall a. Show a => f a -> String) -> Effect Unit
runOutput printResult = do
  log "\n### PrimitiveJsonError Output:"
  runDecoder @f "Decode Int to Int" exampleInt printResult toInt
  runDecoder @f "Decode Int to String" exampleString printResult toInt
  runDecoder @f "Decode Record to Int" exampleRec printResult toInt
  runDecoder @f "Decode Record incorrectly" exampleRec printResult toRecIncorrectly
  log "==="

runDecoder
  :: forall @f a
   . String
  -> Json
  -> (f a -> String)
  -> (Json -> f a)
  -> Effect Unit
runDecoder msg example printer tor =
  log
    $ append ("\n" <> msg <> ":\n")
    $ printer
    $ tor example

toRecIncorrectly :: forall @f. IsJsonDecoder f => Json -> f _
toRecIncorrectly =
  toRecord
    { boolean: toRequired @f toString
    , number: toRequired @f toBoolean
    , string: toRequired @f toNumber
    , int: toRequired @f toString
    , object: toRequired @f toInt
    , array: toRequired @f $ toObject toString
    , record: toRequired @f $ toArray toInt
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
        , bar: fromRequired fromUnitToNull
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

