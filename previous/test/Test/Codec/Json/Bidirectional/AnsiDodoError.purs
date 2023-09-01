module Test.Codec.Json.Bidirectional.AnsiDodoError where

import Prelude

import Codec.Json.Bidirectional.Value as Codec
import Codec.Json.Errors.AnsiDodoError (adeC, decodeAde, printAnsiDodoError)
import Codec.Json.JsonCodec (JsonCodec, encode)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, hush)
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (notNull)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String.NonEmpty.Internal (NonEmptyString(..))
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
  log "\n### Testing decoder error messages:"
  runDecoder "Record to some other record" incorrectRecordValue incorrectRecordCodec recordCodec

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

runDecoder
  :: forall a b
   . Show a
  => String
  -> a
  -> JsonCodec (Doc GraphicsParam) Unit a
  -> JsonCodec (Doc GraphicsParam) Unit b
  -> Effect Unit
runDecoder msg input encoder decoder = do
  let
    j = encode unit input encoder
    error = either (Just <<< printAnsiDodoError) (const Nothing) $ decodeAde j decoder
  log $ "\n" <> msg <> " (bidirectional - error message):"
  for_ error \e ->
    log $ "\n" <> e

recordValue :: _
recordValue =
  { unitType: unit
  , booleanType: true
  , numberType: 1.4
  , intType: 9
  , charType: 'z'
  , stringType: "hello"
  , nonEmptyStringType: NonEmptyString "This is not empty!"
  , nonEmptyStringEmpty: NonEmptyString "This is not empty!"
  , arrayType: Array.range 1 10
  , arrayElem: Array.range 1 10
  , nonEmptyArrayType: NEA.cons' 1 [ 2 ]
  , nonEmptyArrayEmpty: NEA.cons' 1 [ 2 ]
  , nonEmptyArrayElem: NEA.cons' 1 [ 2 ]
  , objectType: Object.singleton "a" "1"
  , objectKey: Object.singleton "a" "1"
  , objectValue: Object.singleton "a" "1"
  , recordType:
      { foo: "what"
      , bar: unit
      }
  , nullableType: notNull 1
  , nullableNull: notNull 1
  , nullableNotNull: notNull 1
  , identityType: Identity 2
  , maybeType: Just 1
  , maybeJust: Just 1
  , maybeNothing: Nothing
  , eitherType: Left 1 :: Either Int Int
  , eitherLeft: Left 1 :: Either Int Int
  , eitherRight: Right 2 :: Either Int Int
  , tupleType: Tuple "a" "b"
  , tuple0: Tuple 1 "fail"
  , tuple1: Tuple "fail" 1
  , theseType: This 1 :: These Int String
  , theseThis: This 1 :: These Int String
  , theseThat: That "foo" :: These Int String
  , theseBoth: Both 1 "foo" :: These Int String
  , nonEmptyType: NonEmpty 1 Nil
  , nonEmptyHead: NonEmpty 1 Nil
  , nonEmptyTail: NonEmpty 1 Nil
  , listType: 1 : 2 : 3 : Nil
  , listElem: 1 : 2 : 3 : Nil
  , nonEmptyListType: NEL.cons' 1 Nil
  , nonEmptyListEmpty: NEL.cons' 1 Nil
  , nonEmptyListElem: NEL.cons' 1 Nil
  , mapType: Map.fromFoldable
      [ Tuple "a" 1
      , Tuple "b" 2
      ]
  , mapKey: Map.fromFoldable
      [ Tuple "a" 1
      , Tuple "b" 2
      ]
  , mapValue: Map.fromFoldable
      [ Tuple "a" 1
      , Tuple "b" 2
      ]
  , setType: Set.fromFoldable
      [ 1, 2, 3, 4 ]
  , setElem: Set.fromFoldable
      [ 1, 2, 3, 4 ]
  , nonEmptySetType: NonEmptySet.singleton 2
  , nonEmptySetEmpty: NonEmptySet.singleton 2
  , nonEmptySetElem: NonEmptySet.singleton 2
  }

recordCodec :: JsonCodec (Doc GraphicsParam) Unit _
recordCodec =
  Codec.record
    { unitType: adeC Codec.unitCodec
    , booleanType: adeC Codec.boolean
    , numberType: adeC Codec.number
    , intType: adeC Codec.int
    , charType: adeC Codec.char
    , stringType: adeC Codec.string
    , nonEmptyStringType: adeC Codec.nonEmptyString
    , nonEmptyStringEmpty: adeC Codec.nonEmptyString
    , arrayType: adeC $ Codec.array Codec.int
    , arrayElem: adeC $ Codec.array Codec.int
    , nonEmptyArrayType: adeC $ Codec.nonEmptyArray Codec.int
    , nonEmptyArrayEmpty: adeC $ Codec.nonEmptyArray Codec.int
    , nonEmptyArrayElem: adeC $ Codec.nonEmptyArray Codec.int
    , objectType: adeC $ Codec.object Codec.string
    , objectKey: adeC $ Codec.object Codec.string
    , objectValue: adeC $ Codec.object Codec.string
    , recordType: adeC $ Codec.record
        { foo: adeC Codec.string
        , bar: adeC Codec.unitCodec
        }
    , nullableType: adeC $ Codec.nullable Codec.int
    , nullableNull: adeC $ Codec.nullable Codec.int
    , nullableNotNull: adeC $ Codec.nullable Codec.int
    , identityType: adeC $ Codec.identityCodec Codec.int
    , maybeType: adeC $ Codec.maybe Codec.int
    , maybeJust: adeC $ Codec.maybe Codec.int
    , maybeNothing: adeC $ Codec.maybe Codec.int
    , eitherType: adeC $ Codec.either Codec.int Codec.int
    , eitherLeft: adeC $ Codec.either Codec.int Codec.int
    , eitherRight: adeC $ Codec.either Codec.int Codec.int
    , tupleType: adeC $ Codec.tuple Codec.string Codec.string
    , tuple0: adeC $ Codec.tuple Codec.int Codec.string
    , tuple1: adeC $ Codec.tuple Codec.string Codec.int
    , theseType: adeC $ Codec.these Codec.int Codec.string
    , theseThis: adeC $ Codec.these Codec.int Codec.string
    , theseThat: adeC $ Codec.these Codec.int Codec.string
    , theseBoth: adeC $ Codec.these Codec.int Codec.string
    , nonEmptyType: adeC $ Codec.nonEmpty Codec.int Codec.list
    , nonEmptyHead: adeC $ Codec.nonEmpty Codec.int Codec.list
    , nonEmptyTail: adeC $ Codec.nonEmpty Codec.int Codec.list
    , listType: adeC $ Codec.list Codec.int
    , listElem: adeC $ Codec.list Codec.int
    , nonEmptyListType: adeC $ Codec.nonEmptyList Codec.int
    , nonEmptyListEmpty: adeC $ Codec.nonEmptyList Codec.int
    , nonEmptyListElem: adeC $ Codec.nonEmptyList Codec.int
    , mapType: adeC $ Codec.mapCodec Codec.string Codec.int
    , mapKey: adeC $ Codec.mapCodec Codec.string Codec.int
    , mapValue: adeC $ Codec.mapCodec Codec.string Codec.int
    , setType: adeC $ Codec.set Codec.int
    , setElem: adeC $ Codec.set Codec.int
    , nonEmptySetType: adeC $ Codec.nonEmptySet Codec.int
    , nonEmptySetEmpty: adeC $ Codec.nonEmptySet Codec.int
    , nonEmptySetElem: adeC $ Codec.nonEmptySet Codec.int
    }

incorrectRecordValue :: _
incorrectRecordValue =
  { unitType: "foo"
  , booleanType: 9
  , numberType: true
  , intType: "hello"
  , charType: false
  , stringType: 4.2
  , nonEmptyStringType: false
  , nonEmptyStringEmpty: ""
  , arrayType: Object.fromFoldable
      [ Tuple "a" "1"
      , Tuple "b" "2"
      , Tuple "c" "3"
      , Tuple "d" "4"
      ]
  , arrayElem:
      [ "a", "b", "c" ]
  , nonEmptyArrayType: Object.fromFoldable
      [ Tuple "a" "1"
      , Tuple "b" "2"
      , Tuple "c" "3"
      , Tuple "d" "4"
      ]
  , nonEmptyArrayEmpty: [] :: Array String
  , nonEmptyArrayElem: [ "a", "b", "c" ]
  , objectType: [ 1, 2, 3 ]
  , objectKey: Object.singleton "z" 4
  , objectValue: Object.singleton "a" 4
  , recordType: Array.range 1 10
  , nullableType: [ 1 ]
  , nullableNull: true
  , nullableNotNull: false
  , identityType: [] :: Array Int
  , maybeType: [] :: Array Int
  , maybeJust: { tag: "Just" }
  , maybeNothing: { tag: "Nope" }
  , eitherType: [] :: Array Int
  , eitherLeft: { tag: "Left" }
  , eitherRight: { tag: "Right" }
  , tupleType: false
  , tuple0: [ "a", "b" ]
  , tuple1: [ "a", "b" ]
  , theseType: [] :: Array String
  , theseThis: { tag: "This" }
  , theseThat: { tag: "That" }
  , theseBoth:
      { tag: "Both"
      , value:
          { this: [] :: Array Int
          , that: [] :: Array Int
          }
      }
  , nonEmptyType: [] :: Array Int
  , nonEmptyHead:
      { tag: "NonEmpty"
      , value: { head: [] }
      }
  , nonEmptyTail:
      { tag: "NonEmpty"
      , value:
          { head: "a"
          , tail: false
          }
      }
  , listType: [] :: Array Int
  , listElem: [ "a", "b" ]
  , nonEmptyListType: false
  , nonEmptyListEmpty: [] :: Array Int
  , nonEmptyListElem: [ "a" ]
  , mapType: [] :: Array Int
  , mapKey:
      [ { key: [ 1 ], value: [ 2 ] } ]
  , mapValue:
      [ { key: [ "a" ], value: [ "foo" ] }
      , { key: [ "b" ], value: [ "bar" ] }
      ]
  , setType: false
  , setElem: [ "a" ]
  , nonEmptySetType: false
  , nonEmptySetEmpty: [] :: Array String
  , nonEmptySetElem: [ "a" ]
  }

incorrectRecordCodec :: JsonCodec (Doc GraphicsParam) Unit _
incorrectRecordCodec =
  Codec.record
    { unitType: adeC Codec.string
    , booleanType: adeC Codec.int
    , numberType: adeC Codec.boolean
    , intType: adeC Codec.string
    , charType: adeC Codec.boolean
    , stringType: adeC Codec.number
    , nonEmptyStringType: adeC Codec.boolean
    , nonEmptyStringEmpty: adeC Codec.string
    , arrayType: adeC $ Codec.object Codec.string
    , arrayElem: adeC $ Codec.array Codec.string
    , nonEmptyArrayType: adeC $ Codec.object Codec.string
    , nonEmptyArrayEmpty: adeC $ Codec.array Codec.string
    , nonEmptyArrayElem: adeC $ Codec.array Codec.string
    , objectType: adeC $ Codec.array Codec.int
    , objectKey: adeC $ Codec.object Codec.int
    , objectValue: adeC $ Codec.object Codec.int
    , recordType: adeC $ Codec.array Codec.int
    , nullableType: adeC $ Codec.array Codec.int
    , nullableNull: adeC Codec.boolean
    , nullableNotNull: adeC Codec.boolean
    , identityType: adeC $ Codec.array Codec.int
    , maybeType: adeC $ Codec.array Codec.int
    , maybeJust: adeC $ Codec.record { tag: adeC Codec.string }
    , maybeNothing: adeC $ Codec.record { tag: adeC Codec.string }
    , eitherType: adeC $ Codec.array Codec.int
    , eitherLeft: adeC $ Codec.record { tag: adeC Codec.string }
    , eitherRight: adeC $ Codec.record { tag: adeC Codec.string }
    , tupleType: adeC $ Codec.boolean
    , tuple0: adeC $ Codec.array Codec.string
    , tuple1: adeC $ Codec.array Codec.string
    , theseType: adeC $ Codec.array Codec.string
    , theseThis: adeC $ Codec.record
        { tag: adeC Codec.string }
    , theseThat: adeC $ Codec.record
        { tag: adeC Codec.string }
    , theseBoth: adeC $ Codec.record
        { tag: adeC Codec.string
        , value: adeC $ Codec.record
            { this: adeC $ Codec.array Codec.int
            , that: adeC $ Codec.array Codec.int
            }
        }
    , nonEmptyType: adeC $ Codec.array Codec.int
    , nonEmptyHead: adeC $ Codec.record
        { tag: adeC Codec.string
        , value: adeC $ Codec.record
            { head: adeC $ Codec.array Codec.int }
        }
    , nonEmptyTail: adeC $ Codec.record
        { tag: adeC Codec.string
        , value: adeC $ Codec.record
            { head: adeC $ Codec.string
            , tail: adeC $ Codec.boolean
            }
        }
    , listType: adeC $ Codec.array Codec.int
    , listElem: adeC $ Codec.array Codec.string
    , nonEmptyListType: adeC $ Codec.boolean
    , nonEmptyListEmpty: adeC $ Codec.array Codec.int
    , nonEmptyListElem: adeC $ Codec.array Codec.string
    , mapType: adeC $ Codec.array Codec.int
    , mapKey: adeC $ Codec.array $ Codec.record
        { key: adeC $ Codec.array Codec.int
        , value: adeC $ Codec.array Codec.int
        }
    , mapValue: adeC $ Codec.array $ Codec.record
        { key: adeC $ Codec.array Codec.string
        , value: adeC $ Codec.array Codec.string
        }
    , setType: adeC Codec.boolean
    , setElem: adeC $ Codec.array Codec.string
    , nonEmptySetType: adeC Codec.boolean
    , nonEmptySetEmpty: adeC $ Codec.array Codec.string
    , nonEmptySetElem: adeC $ Codec.array Codec.string
    }
