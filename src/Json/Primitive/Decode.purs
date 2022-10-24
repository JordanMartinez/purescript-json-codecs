module Json.Primitive.Decode where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, mkFn4, runFn4)
import Data.Newtype (class Newtype, un, unwrap)
import Data.String (Pattern(..), contains)
import Data.Validation.Semigroup (V(..), invalid)
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

data TypeHint
  = TyName String
  | CtorName String
  | Subterm Int
  | Field String

derive instance Eq TypeHint

printTypeHint :: TypeHint -> String
printTypeHint = case _ of
  TyName s -> "while decoding the type, " <> s
  CtorName s -> "while decoding the constructor, " <> s
  Subterm i -> "while decoding the subterm at index, " <> show i
  Field f -> "while decoding the value under the label, " <> f

type JsonErrorHandlers e =
  { append :: e -> e -> e
  , onTypeMismatch :: Array JsonOffset -> ExpectedJsonType -> ActualJsonType -> e
  , onMissingField :: Array JsonOffset -> String -> e
  , onMissingIndex :: Array JsonOffset -> Int -> e
  , onUnrefinableValue :: Array JsonOffset -> String -> e
  , onStructureError :: Array JsonOffset -> String -> e
  , addHint :: Array JsonOffset -> TypeHint -> e -> e
  }

-- | Overview of values:
-- | - json - the JSON value currently being decoded at this point
-- | - pathSoFar - the position within the larger JSON that the current JSON is located
-- | - handlers - runtime-configured way to handling errors
-- | - extra - top-down custom data one may need for writing a decoder. This is where
-- |           local overrides for typeclass instances can be provided.
-- |           If this value isn't needed, you should set this to `Unit`.
newtype JsonDecoderInput e extra = JsonDecoderInput
  { json :: Json
  , pathSoFar :: Array JsonOffset
  , handlers :: JsonErrorHandlers e
  , extra :: extra
  }

-- | Overview of values:
-- | - Json - the JSON value currently being decoded at this point
-- | - Array JsonOffset - the position within the larger JSON that the current JSON is located
-- | - JsonErrorHandlers e - runtime-configured way to handling errors
-- | - extra - top-down custom data one may need for writing a decoder. This is where
-- |           local overrides for typeclass instances can be provided.
-- |           If this value isn't needed, you should set this to `Unit`.
-- | - V e a - an Either-like monad that accumulates errors using the `append` function in the handlers arg.
type JsonDecoderFn e extra a = Fn4 Json (Array JsonOffset) (JsonErrorHandlers e) extra (V e a)

derive instance Newtype (JsonDecoderInput e extra) _

newtype JsonDecoder e extra a = JsonDecoder (JsonDecoderFn e extra a)

instance functorJsonDecoder :: Functor (JsonDecoder e extra) where
  map f (JsonDecoder fn) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
    map f $ runFn4 fn json pathSoFar handlers extra

instance applyJsonDecoder :: Apply (JsonDecoder e extra) where
  apply (JsonDecoder ff) (JsonDecoder fa) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
    case runFn4 ff json pathSoFar handlers extra, runFn4 fa json pathSoFar handlers extra of
      V (Left e1), V (Left e2) -> V (Left $ handlers.append e1 e2)
      V (Left e1), _ -> V (Left e1)
      _, V (Left e2) -> V (Left e2)
      V (Right f'), V (Right a') -> V (Right (f' a'))

instance applicativeJsonDecoder :: Applicative (JsonDecoder e extra) where
  pure a = JsonDecoder $ mkFn4 \_ _ _ _ -> V $ Right a

getPathSoFar :: forall e extra. JsonDecoder e extra (Array JsonOffset)
getPathSoFar = JsonDecoder $ mkFn4 \_ pathSoFar _ _ -> V $ Right pathSoFar

withOffset :: forall e extra a. JsonOffset -> Json -> JsonDecoder e extra a -> JsonDecoder e extra a
withOffset offset json (JsonDecoder f) = JsonDecoder $
  mkFn4 \_ pathSoFar handlers extra ->
    runFn4 f json (Array.snoc pathSoFar offset) handlers extra

onError :: forall e extra a. (Array JsonOffset -> e -> e) -> JsonDecoder e extra a -> JsonDecoder e extra a
onError mapErrs (JsonDecoder f) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
  lmap (mapErrs pathSoFar) $ runFn4 f json pathSoFar handlers extra

failWithMissingField :: forall e extra a. String -> JsonDecoder e extra a
failWithMissingField str = JsonDecoder $ mkFn4 \_ pathSoFar handlers _ ->
  invalid $ handlers.onMissingField pathSoFar str

failWithMissingIndex :: forall e extra a. Int -> JsonDecoder e extra a
failWithMissingIndex idx = JsonDecoder $ mkFn4 \_ pathSoFar handlers _ ->
  invalid $ handlers.onMissingIndex pathSoFar idx

failWithUnrefinableValue :: forall e extra a. String -> JsonDecoder e extra a
failWithUnrefinableValue msg = JsonDecoder $ mkFn4 \_ pathSoFar handlers _ ->
  invalid $ handlers.onUnrefinableValue pathSoFar msg

failWithStructureError :: forall e extra a. String -> JsonDecoder e extra a
failWithStructureError msg = JsonDecoder $ mkFn4 \_ pathSoFar handlers _ ->
  invalid $ handlers.onStructureError pathSoFar msg

addHint :: forall e extra a. TypeHint -> JsonDecoder e extra a -> JsonDecoder e extra a
addHint hint (JsonDecoder f) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
  lmap (handlers.addHint pathSoFar hint) $ runFn4 f json pathSoFar handlers extra

addTypeHint :: forall e extra a. String -> JsonDecoder e extra a -> JsonDecoder e extra a
addTypeHint = addHint <<< TyName

addCtorHint :: forall e extra a. String -> JsonDecoder e extra a -> JsonDecoder e extra a
addCtorHint = addHint <<< CtorName

addSubtermHint :: forall e extra a. Int -> JsonDecoder e extra a -> JsonDecoder e extra a
addSubtermHint = addHint <<< Subterm

addFieldHint :: forall e extra a. String -> JsonDecoder e extra a -> JsonDecoder e extra a
addFieldHint = addHint <<< Field

-- | Works like `alt`/`<|>`. Decodes using the first decoder and, if that fails,
-- | decodes using the second decoder. Errors from both decoders accumulate.
altAccumulate :: forall e extra a. JsonDecoder e extra a -> JsonDecoder e extra a -> JsonDecoder e extra a
altAccumulate (JsonDecoder f1) (JsonDecoder f2) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
  case unwrap $ runFn4 f1 json pathSoFar handlers extra of
    Left e -> case unwrap $ runFn4 f2 json pathSoFar handlers extra of
      Left e2 -> invalid $ handlers.append e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

-- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- | where one is decoding a sum type with a large number of data constructors.
altLast :: forall e extra a. JsonDecoder e extra a -> JsonDecoder e extra a -> JsonDecoder e extra a
altLast (JsonDecoder f1) (JsonDecoder f2) = JsonDecoder $ mkFn4 \json pathSoFar handlers extra ->
  case unwrap $ runFn4 f1 json pathSoFar handlers extra of
    Left _ -> runFn4 f2 json pathSoFar handlers extra
    Right a -> V $ Right a

runJsonDecoder
  :: forall e extra a
   . JsonErrorHandlers e
  -> extra
  -> Json
  -> JsonDecoder e extra a
  -> Either e a
runJsonDecoder handlers extra json (JsonDecoder fn) =
  un V $ runFn4 fn json [] handlers extra
