module Codec.Json.Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Function.Uncurried (Fn2, Fn3)
import Data.String (Pattern(..), contains)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | When decoding a primtive JSON value, this was the type of value we were expecting to decode.
data ExpectedJsonType
  = ExpectedNull
  | ExpectedBoolean
  | ExpectedNumber
  | ExpectedString
  | ExpectedArray
  | ExpectedObject

printExpectedJsonType :: ExpectedJsonType -> String
printExpectedJsonType = case _ of
  ExpectedNull -> "null"
  ExpectedBoolean -> "boolean"
  ExpectedNumber -> "number"
  ExpectedString -> "string"
  ExpectedArray -> "array"
  ExpectedObject -> "object"

-- | When decoding a primtive JSON value, this was the type of value we were actually got.
data ActualJsonType
  = ActualNull
  | ActualBoolean Boolean
  | ActualNumber Number
  | ActualString String
  | ActualArray (Array Json)
  | ActualObject (Object Json)

printActualJsonType :: ActualJsonType -> String
printActualJsonType = case _ of
  ActualNull -> "null"
  ActualBoolean b -> "boolean: " <> show b
  ActualNumber n -> "number: " <> show n
  ActualString s -> "string: " <> show s
  ActualArray a -> "array of length " <> show (Array.length a)
  ActualObject o -> "object with " <> show (Array.length $ Object.keys o) <> " keys"

-- | Indicates the path to the current JSON value within some larger JSON value
data JsonOffset
  = AtKey String
  | AtIndex Int

derive instance Eq JsonOffset

printJsonOffset :: JsonOffset -> String
printJsonOffset = case _ of
  AtKey s -> if contains (Pattern "'") s || contains (Pattern "\"") s then ".`" <> s <> "`" else "." <> s
  AtIndex i -> "[" <> show i <> "]"

printJsonOffsetPath :: Array JsonOffset -> String
printJsonOffsetPath = append "ROOT" <<< foldMap printJsonOffset

typeHintMsg :: String -> String
typeHintMsg = append "while decoding the type, "

ctorHintMsg :: String -> String
ctorHintMsg = append "while decoding the constructor, "

subtermHintMsg :: Int -> String
subtermHintMsg = append "while decoding the subterm at index, " <<< show

fieldHintMsg :: String -> String
fieldHintMsg = append "while decoding the value under the label, "

newtype JsonErrorHandlers e = JsonErrorHandlers
  { onTypeMismatch :: Fn3 (Array JsonOffset) ExpectedJsonType ActualJsonType e
  , onMissingField :: Fn2 (Array JsonOffset) String e
  , onMissingIndex :: Fn2 (Array JsonOffset) Int e
  , onUnrefinableValue :: Fn2 (Array JsonOffset) String e
  , onStructureError :: Fn2 (Array JsonOffset) String e
  , addJsonOffset :: Fn2 (Array JsonOffset) JsonOffset (Array JsonOffset)
  , addTypeHint :: Fn3 (Array JsonOffset) String e e
  , addCtorHint :: Fn3 (Array JsonOffset) String e e
  , addSubtermHint :: Fn3 (Array JsonOffset) Int e e
  , addFieldHint :: Fn3 (Array JsonOffset) String e e
  }
