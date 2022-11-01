module Json.JsonDecoder where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Data.Argonaut.Core (Json)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn5, runFn5)
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

newtype JsonErrorHandlers e = JsonErrorHandlers
  { onTypeMismatch :: Array JsonOffset -> ExpectedJsonType -> ActualJsonType -> e
  , onMissingField :: Array JsonOffset -> String -> e
  , onMissingIndex :: Array JsonOffset -> Int -> e
  , onUnrefinableValue :: Array JsonOffset -> String -> e
  , onStructureError :: Array JsonOffset -> String -> e
  , includeJsonOffset :: Boolean
  , addHint :: Array JsonOffset -> TypeHint -> e -> e
  }

-- | Overview of values:
-- | - Json - the JSON value currently being decoded at this point
-- | - Array JsonOffset - the position within the larger JSON that the current JSON is located
-- | - JsonErrorHandlers e - runtime-configured way to handling errors
-- | - extra - top-down custom data one may need for writing a decoder. This is where
-- |           local overrides for typeclass instances can be provided.
-- |           If this value isn't needed, you should set this to `Unit`.
-- | - V e a - an Either-like monad that accumulates errors using the `append` function in the handlers arg.
type JsonDecoder e extra a = DecoderFn Json (Array JsonOffset) (JsonErrorHandlers e) e extra a

addOffset :: forall e extra a. JsonOffset -> Json -> JsonDecoder e extra a -> JsonDecoder e extra a
addOffset offset json (DecoderFn f) = DecoderFn $
  mkFn5 \_ path appendFn handlers@(JsonErrorHandlers h) extra ->
    runFn5 f json (if h.includeJsonOffset then Array.snoc path offset else path) appendFn handlers extra

onError :: forall e extra a. (Array JsonOffset -> e -> e) -> JsonDecoder e extra a -> JsonDecoder e extra a
onError mapErrs (DecoderFn f) = DecoderFn $ mkFn5 \json path appendFn handlers extra ->
  lmap (mapErrs path) $ runFn5 f json path appendFn handlers extra

failWithMissingField :: forall e extra a. String -> JsonDecoder e extra a
failWithMissingField str = DecoderFn $ mkFn5 \_ path _ (JsonErrorHandlers h) _ ->
  invalid $ h.onMissingField path str

failWithMissingIndex :: forall e extra a. Int -> JsonDecoder e extra a
failWithMissingIndex idx = DecoderFn $ mkFn5 \_ path _ (JsonErrorHandlers h) _ ->
  invalid $ h.onMissingIndex path idx

failWithUnrefinableValue :: forall e extra a. String -> JsonDecoder e extra a
failWithUnrefinableValue msg = DecoderFn $ mkFn5 \_ path _ (JsonErrorHandlers h) _ ->
  invalid $ h.onUnrefinableValue path msg

failWithStructureError :: forall e extra a. String -> JsonDecoder e extra a
failWithStructureError msg = DecoderFn $ mkFn5 \_ path _ (JsonErrorHandlers h) _ ->
  invalid $ h.onStructureError path msg

addHint :: forall e extra a. TypeHint -> JsonDecoder e extra a -> JsonDecoder e extra a
addHint hint (DecoderFn f) = DecoderFn $ mkFn5 \json path appendFn handlers@(JsonErrorHandlers h) extra ->
  lmap (h.addHint path hint) $ runFn5 f json path appendFn handlers extra

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
altAccumulate (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \json path appendFn handlers extra ->
  case unwrap $ runFn5 f1 json path appendFn handlers extra of
    Left e -> case unwrap $ runFn5 f2 json path appendFn handlers extra of
      Left e2 -> invalid $ appendFn e e2
      Right a -> V $ Right a
    Right a -> V $ Right a

-- | Same as `altAccumulate` except only the last error is kept. Helpful in cases
-- | where one is decoding a sum type with a large number of data constructors.
altLast :: forall e extra a. JsonDecoder e extra a -> JsonDecoder e extra a -> JsonDecoder e extra a
altLast (DecoderFn f1) (DecoderFn f2) = DecoderFn $ mkFn5 \json path appendFn handlers extra ->
  case unwrap $ runFn5 f1 json path appendFn handlers extra of
    Left _ -> runFn5 f2 json path appendFn handlers extra
    Right a -> V $ Right a

runJsonDecoder
  :: forall e extra a
   . JsonErrorHandlers e
  -> (e -> e -> e)
  -> extra
  -> Json
  -> JsonDecoder e extra a
  -> Either e a
runJsonDecoder handlers appendFn extra json (DecoderFn fn) =
  un V $ runFn5 fn json [] appendFn handlers extra
