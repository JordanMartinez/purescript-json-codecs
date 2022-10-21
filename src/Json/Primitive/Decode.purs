module Json.Primitive.Decode where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Core (Json, caseJson)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap)
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
  , withHint :: Array JsonOffset -> TypeHint -> e -> e
  }

type JsonDecoderInput e =
  { json :: Json
  , pathSoFar :: Array JsonOffset
  , handlers :: JsonErrorHandlers e
  }

newtype JsonDecoder e a = JsonDecoder (ReaderT (JsonDecoderInput e) (V e) a)

derive newtype instance functorJsonDecoder :: Functor (JsonDecoder e)
instance applyJsonDecoder :: Apply (JsonDecoder e) where
  apply (JsonDecoder (ReaderT ff)) (JsonDecoder (ReaderT fa)) = JsonDecoder $ ReaderT \a ->
    case fa a, ff a of
      V (Left e1), V (Left e2) -> V (Left $ a.handlers.append e1 e2)
      V (Left e1), _ -> V (Left e1)
      _, V (Left e2) -> V (Left e2)
      V (Right a'), V (Right f') -> V (Right (f' a'))

instance applicativeJsonDecoder :: Applicative (JsonDecoder e) where
  pure a = JsonDecoder $ ReaderT \_ -> V $ Right a

-- TODO: add `Alt` instance to `V`
-- derive newtype instance altJsonDecoder :: Semigroup e => Alt (JsonDecoder e)

getPathSoFar :: forall e. JsonDecoder e (Array JsonOffset)
getPathSoFar = JsonDecoder $ ReaderT \r -> V $ Right r.pathSoFar

withOffset :: forall e a. JsonOffset -> Json -> JsonDecoder e a -> JsonDecoder e a
withOffset offset json (JsonDecoder (ReaderT f)) = JsonDecoder $ ReaderT \r -> f $ r { json = json, pathSoFar = Array.snoc r.pathSoFar offset }

onError :: forall e a. (Array JsonOffset -> e -> e) -> JsonDecoder e a -> JsonDecoder e a
onError mapErrs (JsonDecoder (ReaderT f)) = JsonDecoder $ ReaderT \input@{ pathSoFar } ->
  lmap (mapErrs pathSoFar) $ f input

failWithMissingField :: forall e a. String -> JsonDecoder e a
failWithMissingField str = JsonDecoder $ ReaderT \input ->
  invalid $ input.handlers.onMissingField input.pathSoFar str

failWithMissingIndex :: forall e a. Int -> JsonDecoder e a
failWithMissingIndex idx = JsonDecoder $ ReaderT \input ->
  invalid $ input.handlers.onMissingIndex input.pathSoFar idx

failWithUnrefinableValue :: forall e a. String -> JsonDecoder e a
failWithUnrefinableValue msg = JsonDecoder $ ReaderT \input ->
  invalid $ input.handlers.onUnrefinableValue input.pathSoFar msg

failWithStructureError :: forall e a. String -> JsonDecoder e a
failWithStructureError msg = JsonDecoder $ ReaderT \input ->
  invalid $ input.handlers.onStructureError input.pathSoFar msg

withHint :: forall err a. TypeHint -> JsonDecoder err a -> JsonDecoder err a
withHint hint (JsonDecoder (ReaderT f)) = JsonDecoder $ ReaderT \input ->
  lmap (input.handlers.withHint input.pathSoFar hint) $ f input

addTypeHint :: forall e a. String -> JsonDecoder e a -> JsonDecoder e a
addTypeHint = withHint <<< TyName

addCtorHint :: forall e a. String -> JsonDecoder e a -> JsonDecoder e a
addCtorHint = withHint <<< CtorName

addSubtermHint :: forall e a. Int -> JsonDecoder e a -> JsonDecoder e a
addSubtermHint = withHint <<< Subterm

addFieldHint :: forall e a. String -> JsonDecoder e a -> JsonDecoder e a
addFieldHint = withHint <<< Field

-- | Temporarily needed due to `V` not having an `Alt` instance
alt :: forall e a. JsonDecoder e a -> JsonDecoder e a -> JsonDecoder e a
alt (JsonDecoder (ReaderT f1)) (JsonDecoder (ReaderT f2)) = JsonDecoder $ ReaderT \input ->
  case unwrap $ f1 input of
    Left e -> case unwrap $ f2 input of
      Left e2 -> invalid $ input.handlers.append e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

runJsonDecoder
  :: forall e a
   . JsonErrorHandlers e
  -> Json
  -> JsonDecoder e a
  -> Either e a
runJsonDecoder handlers json (JsonDecoder reader) =
  un V $ runReaderT reader { handlers, json, pathSoFar: [] }

decodeNull :: forall e. JsonDecoder e Unit
decodeNull = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (V <<< Right)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNull <<< ActualBoolean)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNull <<< ActualNumber)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNull <<< ActualString)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNull <<< ActualArray)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNull <<< ActualObject)
    json

decodeBoolean :: forall e. JsonDecoder e Boolean
decodeBoolean = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (const $ invalid $ handlers.onTypeMismatch pathSoFar ExpectedBoolean ActualNull)
    (V <<< Right)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualNumber)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualString)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualArray)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualObject)
    json

decodeNumber :: forall e. JsonDecoder e Number
decodeNumber = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (const $ invalid $ handlers.onTypeMismatch pathSoFar ExpectedNumber ActualNull)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNumber <<< ActualBoolean)
    (V <<< Right)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNumber <<< ActualString)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNumber <<< ActualArray)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedNumber <<< ActualObject)
    json

decodeString :: forall e. JsonDecoder e String
decodeString = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (const $ invalid $ handlers.onTypeMismatch pathSoFar ExpectedString ActualNull)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedString <<< ActualBoolean)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedString <<< ActualNumber)
    (V <<< Right)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedString <<< ActualArray)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedString <<< ActualObject)
    json

decodeArrayPrim :: forall e. JsonDecoder e (Array Json)
decodeArrayPrim = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (const $ invalid $ handlers.onTypeMismatch pathSoFar ExpectedArray ActualNull)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedArray <<< ActualBoolean)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedArray <<< ActualNumber)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedArray <<< ActualString)
    (V <<< Right)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedArray <<< ActualObject)
    json

decodeIndex :: forall e a. Array Json -> Int -> JsonDecoder e a -> JsonDecoder e a
decodeIndex arr idx = decodeIndex' arr idx do
  JsonDecoder $ ReaderT \{ pathSoFar, handlers } ->
    invalid $ handlers.onMissingIndex pathSoFar idx

decodeIndex' :: forall e a. Array Json -> Int -> JsonDecoder e a -> JsonDecoder e a -> JsonDecoder e a
decodeIndex' arr idx onMissingIndex decodeElem = case Array.index arr idx of
  Nothing ->
    onMissingIndex
  Just a ->
    withOffset (AtIndex idx) a decodeElem

decodeObjectPrim :: forall e. JsonDecoder e (Object Json)
decodeObjectPrim = JsonDecoder $ ReaderT \{ json, pathSoFar, handlers } ->
  caseJson
    (const $ invalid $ handlers.onTypeMismatch pathSoFar ExpectedObject ActualNull)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedObject <<< ActualBoolean)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedObject <<< ActualNumber)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedObject <<< ActualString)
    (invalid <<< handlers.onTypeMismatch pathSoFar ExpectedObject <<< ActualArray)
    (V <<< Right)
    json

decodeField :: forall e a. Object Json -> String -> JsonDecoder e a -> JsonDecoder e a
decodeField obj field = decodeField' obj field do
  JsonDecoder $ ReaderT \{ pathSoFar, handlers } ->
    invalid $ handlers.onMissingField pathSoFar field

decodeField' :: forall e a. Object Json -> String -> JsonDecoder e a -> JsonDecoder e a -> JsonDecoder e a
decodeField' obj field onMissingField decodeElem = case Object.lookup field obj of
  Nothing ->
    onMissingField
  Just a ->
    withOffset (AtKey field) a decodeElem
