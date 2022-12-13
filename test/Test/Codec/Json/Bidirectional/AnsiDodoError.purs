module Test.Codec.Json.Bidirectional.AnsiDodoError where

import Prelude

import Codec.Json.Bidirectional.Value as Codec
import Codec.Json.Errors.AnsiDodoError (adeC, decodeAde, printAnsiDodoError)
import Codec.Json.JsonCodec (JsonCodec, encode)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo.Ansi (GraphicsParam)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object

runOutput :: Effect Unit
runOutput = do
  log "\n### AnsiDodoError Output:"
  runCodec "Int" 1 Codec.int
  runCodec "Record" recordValue recordCodec

runCodec
  :: forall a
   . Show a
  => Eq a
  => String
  -> a
  -> JsonCodec (Doc GraphicsParam) Unit a
  -> Effect Unit
runCodec msg input codec = do
  let
    inputJson = encode unit input codec
    decodeVal j = decodeAde j codec
  log $ "\n" <> msg <> " (bidirectional):"
  log $ "\n  Input:  " <> show input
  log $ "\n  Json:   " <> stringifyWithIndent 2 inputJson
  log $ "\n  Output: " <> (either printAnsiDodoError show $ decodeVal inputJson)
  log $ "\n  Input == Output? " <> (if Just input == (hush $ decodeVal inputJson) then "Yes" else "No")
  log "=======\n"

recordValue :: _
recordValue =
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
  , maybe1: Just 1
  , maybe2: Nothing
  , left: Left 1
  , right: Right 2
  , tuple: Tuple "a" "b"
  , these1: This 1
  , these2: That 2
  , these3: Both 1 2
  , list: 1 : 2 : 3 : Nil
  , map: Map.fromFoldable
      [ Tuple "a" 1
      , Tuple "b" 2
      ]
  , set: Set.fromFoldable
      [ 1, 2, 3, 4 ]
  }

recordCodec :: JsonCodec (Doc GraphicsParam) Unit _
recordCodec =
  Codec.record
    { boolean: adeC Codec.boolean
    , number: adeC Codec.number
    , string: adeC Codec.string
    , int: adeC Codec.int
    , object: adeC $ Codec.object Codec.string
    , array: adeC $ Codec.array Codec.int
    , record: adeC $ Codec.record
        { foo: adeC Codec.string
        , bar: adeC Codec.unitCodec
        }
    , maybe1: adeC $ Codec.maybe Codec.int
    , maybe2: adeC $ Codec.maybe Codec.int
    , left: adeC $ Codec.either Codec.int Codec.int
    , right: adeC $ Codec.either Codec.int Codec.int
    , tuple: adeC $ Codec.tuple Codec.string Codec.string
    , these1: adeC $ Codec.these Codec.int Codec.int
    , these2: adeC $ Codec.these Codec.int Codec.int
    , these3: adeC $ Codec.these Codec.int Codec.int
    , list: adeC $ Codec.list Codec.int
    , map: adeC $ Codec.mapCodec Codec.string Codec.int
    , set: adeC $ Codec.set Codec.int
    }
