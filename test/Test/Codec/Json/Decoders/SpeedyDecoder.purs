module Test.Codec.Json.Decoders.SpeedyDecoder where

import Prelude

import Codec.Json.Decoders.PlainPrettyDecoder (PlainPrettyDecoder, runPlainPrettyDecoder)
import Codec.Json.Decoders.SpeedyDecoder (decodeWithErr)
import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Codec.Json.Unidirectional.Value (fromArray, fromInt, fromMap, fromRecord, fromRequired, fromString, toArray, toInt, toMap, toRecord, toRequired, toString)
import Data.Argonaut.Core (Json)
import Data.Either (either)
import Data.Map (Map)
import Data.Map as Map
import Dodo as D
import Effect (Effect)
import Effect.Class.Console (log)

runTest :: Effect Unit
runTest = do
  let
    logPrint = log <<< either (D.print D.plainText D.twoSpaces) show <<< runPlainPrettyDecoder
  logPrint $ decodeWithErr @PlainPrettyDecoder toValue exampleRec
  logPrint $ decodeWithErr @PlainPrettyDecoder toValue exampleBadRec

toValue :: forall @f. IsJsonDecoder f => Json -> f { a :: { b :: Array { c :: Map Int String } } }
toValue = toRecord @f
  { a: toRequired @f $ toRecord
      { b: toRequired @f $ toArray $ toRecord
          { c: toRequired @f $ toMap toInt toString }
      }
  }

exampleRec :: Json
exampleRec =
  fromRecord fromR value
  where
  fromR =
    { a: fromRequired $ fromRecord
        { b: fromRequired $ fromArray $ fromRecord
            { c: fromRequired $ fromMap fromInt fromString }
        }
    }
  value =
    { a:
        { b:
            [ { c: Map.singleton 4 "bar" }
            , { c: Map.singleton 4 "bar" }
            ]
        }
    }

exampleBadRec :: Json
exampleBadRec =
  fromRecord fromR value
  where
  fromR =
    { a: fromRequired $ fromRecord
        { b: fromRequired $ fromArray $ fromRecord
            { c: fromRequired $ fromMap fromInt fromInt }
        }
    }
  value =
    { a:
        { b:
            [ { c: Map.singleton 4 6 }
            , { c: Map.singleton 4 6 }
            ]
        }
    }
