module Json.Primitive.Decode where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Core (Json, caseJson)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.String (Pattern(..), contains)
import Data.Validation.Semigroup (V, invalid)
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

-- | `e` represents a type that can be used as the `err` type in `JsonDecoder`
class Semigroup e <= IsDecodeJsonError e where
  onTypeMismatch :: Array JsonOffset -> ExpectedJsonType -> ActualJsonType -> e

  onMissingField :: Array JsonOffset -> String -> e

  onMissingIndex :: Array JsonOffset -> Int -> e

  onUnrefinableValue :: Array JsonOffset -> String -> e

  onStructureError :: Array JsonOffset -> String -> e

  withHint :: forall a. TypeHint -> JsonDecoder e a -> JsonDecoder e a

addTypeHint :: forall err a. IsDecodeJsonError err => String -> JsonDecoder err a -> JsonDecoder err a
addTypeHint = withHint <<< TyName

addCtorHint :: forall err a. IsDecodeJsonError err => String -> JsonDecoder err a -> JsonDecoder err a
addCtorHint = withHint <<< CtorName

addSubtermHint :: forall err a. IsDecodeJsonError err => Int -> JsonDecoder err a -> JsonDecoder err a
addSubtermHint = withHint <<< Subterm

addFieldHint :: forall err a. IsDecodeJsonError err => String -> JsonDecoder err a -> JsonDecoder err a
addFieldHint = withHint <<< Field

type JsonDecoderInput =
  { json :: Json
  , pathSoFar :: Array JsonOffset
  }

newtype JsonDecoder err a = JsonDecoder (ReaderT JsonDecoderInput (V err) a)

derive instance Newtype (JsonDecoder err a) _
derive newtype instance functorJsonDecoder :: Functor (JsonDecoder err)
derive newtype instance applyJsonDecoder :: Semigroup err => Apply (JsonDecoder err)
derive newtype instance applicativeJsonDecoder :: Semigroup err => Applicative (JsonDecoder err)
derive newtype instance semigroupJsonDecoder :: (Semigroup err, Semigroup a) => Semigroup (JsonDecoder err a)

-- TODO: add `Alt` instance to `V`
-- derive newtype instance altJsonDecoder :: Semigroup err => Alt (JsonDecoder err)

getPathSoFar :: forall err. Semigroup err => JsonDecoder err (Array JsonOffset)
getPathSoFar = JsonDecoder $ ReaderT \r -> pure r.pathSoFar

withOffset :: forall err a. Semigroup err => JsonOffset -> Json -> JsonDecoder err a -> JsonDecoder err a
withOffset offset json = over JsonDecoder \(ReaderT f) -> ReaderT \r -> f $ r { json = json, pathSoFar = Array.snoc r.pathSoFar offset }

onError :: forall err a. (Array JsonOffset -> err -> err) -> JsonDecoder err a -> JsonDecoder err a
onError mapErrs (JsonDecoder (ReaderT f)) = JsonDecoder $ ReaderT \input@{ pathSoFar } ->
  lmap (mapErrs pathSoFar) $ f input

fail :: forall err a. err -> JsonDecoder err a
fail = JsonDecoder <<< ReaderT <<< const <<< invalid

failWithPath :: forall err a. (Array JsonOffset -> err) -> JsonDecoder err a
failWithPath buildError = JsonDecoder $ ReaderT \{ pathSoFar } -> invalid $ buildError pathSoFar

-- | Temporarily needed due to `V` not having an `Alt` instance
alt :: forall err a. Semigroup err => JsonDecoder err a -> JsonDecoder err a -> JsonDecoder err a
alt (JsonDecoder (ReaderT f1)) (JsonDecoder (ReaderT f2)) = JsonDecoder $ ReaderT \a ->
  case unwrap $ f1 a of
    Left e -> case unwrap $ f2 a of
      Left e2 -> invalid $ e <> e2
      Right a' -> pure a'
    Right a' -> pure a'

runJsonDecoder
  :: forall err a
   . Json
  -> JsonDecoder err a
  -> Either err a
runJsonDecoder json decoder =
  unwrap $ runJsonDecoder' decoder { json, pathSoFar: [] }

runJsonDecoder' :: forall err a. JsonDecoder err a -> JsonDecoderInput -> V err a
runJsonDecoder' (JsonDecoder f) input = runReaderT f input

decodeNull :: forall err. IsDecodeJsonError err => JsonDecoder err Unit
decodeNull = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    pure
    (invalid <<< onTypeMismatch pathSoFar ExpectedNull <<< ActualBoolean)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNull <<< ActualNumber)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNull <<< ActualString)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNull <<< ActualArray)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNull <<< ActualObject)
    json

decodeBoolean :: forall err. IsDecodeJsonError err => JsonDecoder err Boolean
decodeBoolean = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    (const $ invalid $ onTypeMismatch pathSoFar ExpectedBoolean ActualNull)
    pure
    (invalid <<< onTypeMismatch pathSoFar ExpectedBoolean <<< ActualNumber)
    (invalid <<< onTypeMismatch pathSoFar ExpectedBoolean <<< ActualString)
    (invalid <<< onTypeMismatch pathSoFar ExpectedBoolean <<< ActualArray)
    (invalid <<< onTypeMismatch pathSoFar ExpectedBoolean <<< ActualObject)
    json

decodeNumber :: forall err. IsDecodeJsonError err => JsonDecoder err Number
decodeNumber = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    (const $ invalid $ onTypeMismatch pathSoFar ExpectedNumber ActualNull)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNumber <<< ActualBoolean)
    pure
    (invalid <<< onTypeMismatch pathSoFar ExpectedNumber <<< ActualString)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNumber <<< ActualArray)
    (invalid <<< onTypeMismatch pathSoFar ExpectedNumber <<< ActualObject)
    json

decodeString :: forall err. IsDecodeJsonError err => JsonDecoder err String
decodeString = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    (const $ invalid $ onTypeMismatch pathSoFar ExpectedString ActualNull)
    (invalid <<< onTypeMismatch pathSoFar ExpectedString <<< ActualBoolean)
    (invalid <<< onTypeMismatch pathSoFar ExpectedString <<< ActualNumber)
    pure
    (invalid <<< onTypeMismatch pathSoFar ExpectedString <<< ActualArray)
    (invalid <<< onTypeMismatch pathSoFar ExpectedString <<< ActualObject)
    json

decodeArrayPrim :: forall err. IsDecodeJsonError err => JsonDecoder err (Array Json)
decodeArrayPrim = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    (const $ invalid $ onTypeMismatch pathSoFar ExpectedArray ActualNull)
    (invalid <<< onTypeMismatch pathSoFar ExpectedArray <<< ActualBoolean)
    (invalid <<< onTypeMismatch pathSoFar ExpectedArray <<< ActualNumber)
    (invalid <<< onTypeMismatch pathSoFar ExpectedArray <<< ActualString)
    pure
    (invalid <<< onTypeMismatch pathSoFar ExpectedArray <<< ActualObject)
    json

decodeIndex :: forall err a. IsDecodeJsonError err => Array Json -> Int -> JsonDecoder err a -> JsonDecoder err a
decodeIndex arr idx = decodeIndex' arr idx do
  JsonDecoder $ ReaderT \{ pathSoFar } ->
    invalid $ onMissingIndex pathSoFar idx

decodeIndex' :: forall err a. IsDecodeJsonError err => Array Json -> Int -> JsonDecoder err a -> JsonDecoder err a -> JsonDecoder err a
decodeIndex' arr idx onMissingIndex decodeElem = case Array.index arr idx of
  Nothing ->
    onMissingIndex
  Just a ->
    withOffset (AtIndex idx) a decodeElem

decodeObjectPrim :: forall err. IsDecodeJsonError err => JsonDecoder err (Object Json)
decodeObjectPrim = JsonDecoder $ ReaderT \{ json, pathSoFar } ->
  caseJson
    (const $ invalid $ onTypeMismatch pathSoFar ExpectedObject ActualNull)
    (invalid <<< onTypeMismatch pathSoFar ExpectedObject <<< ActualBoolean)
    (invalid <<< onTypeMismatch pathSoFar ExpectedObject <<< ActualNumber)
    (invalid <<< onTypeMismatch pathSoFar ExpectedObject <<< ActualString)
    (invalid <<< onTypeMismatch pathSoFar ExpectedObject <<< ActualArray)
    pure
    json

decodeField :: forall err a. IsDecodeJsonError err => Object Json -> String -> JsonDecoder err a -> JsonDecoder err a
decodeField obj field = decodeField' obj field do
  JsonDecoder $ ReaderT \{ pathSoFar } ->
    invalid $ onMissingField pathSoFar field

decodeField' :: forall err a. IsDecodeJsonError err => Object Json -> String -> JsonDecoder err a -> JsonDecoder err a -> JsonDecoder err a
decodeField' obj field onMissingField decodeElem = case Object.lookup field obj of
  Nothing ->
    onMissingField
  Just a ->
    withOffset (AtKey field) a decodeElem
