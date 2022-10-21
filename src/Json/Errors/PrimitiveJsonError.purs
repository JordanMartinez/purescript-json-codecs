module Json.Errors.PrimitiveJsonError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (bifoldMap)
import Data.Either (Either(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over, unwrap)
import Data.These (These(..))
import Json.Errors.Tree (TreeError(..))
import Json.Primitive.Decode (ActualJsonType, ExpectedJsonType, JsonDecoder, JsonErrorHandlers, JsonOffset, TypeHint, printActualJsonType, printExpectedJsonType, printJsonOffsetPath, printTypeHint, runJsonDecoder)

data JsonLeafError
  = TypeMismatch ExpectedJsonType ActualJsonType
  | MissingField String
  | MissingIndex Int
  | UnrefinableValue String
  | StructureError String

printJsonLeafError :: JsonLeafError -> String
printJsonLeafError = case _ of
  TypeMismatch exp act -> printTypeMismatchErr exp act
  MissingField field -> printMissingField field
  MissingIndex idx -> printMissingIndex idx
  UnrefinableValue str -> str
  StructureError str -> str

printTypeMismatchErr :: ExpectedJsonType -> ActualJsonType -> String
printTypeMismatchErr exp act =
  "Expected " <> printExpectedJsonType exp <> " but got " <> printActualJsonType act

printMissingField :: String -> String
printMissingField str =
  "Failed to decode a value under the field `" <> str <> "` because the field did not exist."

printMissingIndex :: Int -> String
printMissingIndex idx =
  "Failed to decode a value under the index `" <> show idx <> "` because the element did not exist."

newtype PrimitiveJsonError = PrimitiveJsonError
  ( TreeError
      { path :: Array JsonOffset, hint :: TypeHint }
      { path :: Array JsonOffset, error :: JsonLeafError }
  )

derive instance Newtype PrimitiveJsonError _
derive newtype instance Semigroup PrimitiveJsonError

pjeHandlers :: JsonErrorHandlers PrimitiveJsonError
pjeHandlers =
  { append: (<>)
  , onTypeMismatch: \path exp -> PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< TypeMismatch exp
  , onMissingField: \path -> PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< MissingField
  , onMissingIndex: \path -> PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< MissingIndex
  , onUnrefinableValue: \path -> PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< UnrefinableValue
  , onStructureError: \path -> PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< StructureError
  , withHint: \path hint -> over PrimitiveJsonError case _ of
      TreeError (Left (That x)) -> TreeError $ Left $ Both { path, hint } x
      x -> TreeError $ Left $ Both { path, hint } $ NEA.singleton x
  }

printPrimitiveJsonError :: PrimitiveJsonError -> String
printPrimitiveJsonError =
  Array.intercalate "\n"
    <<< bifoldMap
      ( \{ path, hint } -> do
          let indent = indentByPathLength path
          [ indent <> printTypeHint hint
          , indent <> printPath path
          ]
      )
      ( \{ path, error } -> do
          let indent = indentByPathLength path
          [ indent <> printJsonLeafError error
          , indent <> printPath path
          ]
      )
    <<< unwrap
  where
  printPath p = "  at path: " <> printJsonOffsetPath p
  indentByPathLength p = power "  " $ Array.length p

runJsonDecoderPJE :: forall a. Json -> JsonDecoder PrimitiveJsonError a -> Either PrimitiveJsonError a
runJsonDecoderPJE = runJsonDecoder pjeHandlers

pje :: forall a. JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
pje = identity
