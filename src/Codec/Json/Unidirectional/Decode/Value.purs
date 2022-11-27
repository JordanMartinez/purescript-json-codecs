module Codec.Json.Unidirectional.Decode.Value
  ( decodeVoid
  , decodeJNull
  , decodeUnitFromNull
  , decodeUnitFromAny
  , decodeBoolean
  , decodeNumber
  , decodeInt
  , decodeChar
  , decodeString
  , decodeNonEmptyString
  , decodeArray
  , decodeJArray
  , decodeIndex
  , decodeIndex'
  , decodeNonEmptyArray
  , decodeObject
  , decodeJObject
  , decodeField
  , decodeField'
  , decodeNullable
  , decodeIdentity
  , decodeMaybeTagged
  , decodeMaybeNullable
  , decodeEither
  , decodeTuple
  , decodeThese
  , decodeNonEmpty
  , decodeList
  , decodeNonEmptyList
  , decodeMap
  , decodeSet
  , decodeNonEmptySet
  , decodeCodePoint
  , PropDecoder
  , RLRecordDecoder
  , RLRecordDecoderBuilder
  , decodeRecord
  , decodeRecord'
  , decodeRecordPrim
  , buildRecordDecoder
  , decodeRequiredProp
  , decodeOptionalProp
  , decodeRequiredProps
  , decodeOptionalProps
  , class InsertRequiredPropDecoders
  , insertRequiredPropDecoders
  , class InsertOptionalPropDecoders
  , insertOptionalPropDecoders
  , class DecodeRowList
  , decodeRowList
  ) where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Codec.Decoder.Qualified as Decoder
import Codec.Json.Errors.DecodeMessages (arrayNotEmptyFailure, numToIntConversionFailure, stringNotEmptyFailure, stringToCharConversionFailure)
import Codec.Json.JsonDecoder (JsonDecoder, JsonDecoder', addCtorHint, addOffset, addSubtermHint, addTypeHint, altAccumulate, failWithMissingField, failWithStructureError, failWithUnrefinableValue)
import Codec.Json.Types (ActualJsonType(..), ExpectedJsonType(..), JsonErrorHandlers(..), JsonOffset(..))
import Data.Argonaut.Core (Json, caseJson)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn5, runFn5)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, codePointAt)
import Data.String.CodeUnits (charAt)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These(..))
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), invalid)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder, buildFromScratch)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

decodeJNull :: forall e extra. JsonDecoder e extra Unit
decodeJNull = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (V <<< Right)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNull <<< ActualBoolean)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNull <<< ActualNumber)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNull <<< ActualString)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNull <<< ActualArray)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNull <<< ActualObject)
    json

decodeBoolean :: forall e extra. JsonDecoder e extra Boolean
decodeBoolean = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (const $ invalid $ h.onTypeMismatch pathSoFar ExpectedBoolean ActualNull)
    (V <<< Right)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualNumber)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualString)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualArray)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedBoolean <<< ActualObject)
    json

decodeNumber :: forall e extra. JsonDecoder e extra Number
decodeNumber = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (const $ invalid $ h.onTypeMismatch pathSoFar ExpectedNumber ActualNull)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNumber <<< ActualBoolean)
    (V <<< Right)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNumber <<< ActualString)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNumber <<< ActualArray)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedNumber <<< ActualObject)
    json

decodeString :: forall e extra. JsonDecoder e extra String
decodeString = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (const $ invalid $ h.onTypeMismatch pathSoFar ExpectedString ActualNull)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedString <<< ActualBoolean)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedString <<< ActualNumber)
    (V <<< Right)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedString <<< ActualArray)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedString <<< ActualObject)
    json

decodeJArray :: forall e extra. JsonDecoder e extra (Array Json)
decodeJArray = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (const $ invalid $ h.onTypeMismatch pathSoFar ExpectedArray ActualNull)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedArray <<< ActualBoolean)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedArray <<< ActualNumber)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedArray <<< ActualString)
    (V <<< Right)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedArray <<< ActualObject)
    json

decodeIndex :: forall e extra a. Int -> JsonDecoder e extra a -> JsonDecoder' e extra (Array Json) a
decodeIndex idx = decodeIndex' idx do
  DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ _ ->
    invalid $ h.onMissingIndex pathSoFar idx

decodeIndex' :: forall e extra a. Int -> JsonDecoder' e extra (Array Json) a -> JsonDecoder e extra a -> JsonDecoder' e extra (Array Json) a
decodeIndex' idx (DecoderFn onMissingIndex) (DecoderFn decodeElem) =
  DecoderFn $
    mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra arr ->
      case Array.index arr idx of
        Nothing ->
          runFn5 onMissingIndex path appendFn handlers extra arr
        Just elemJson ->
          runFn5 decodeElem (if h.includeJsonOffset then Array.snoc path (AtIndex idx) else path) appendFn handlers extra elemJson

decodeJObject :: forall e extra. JsonDecoder e extra (Object Json)
decodeJObject = DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ json ->
  caseJson
    (const $ invalid $ h.onTypeMismatch pathSoFar ExpectedObject ActualNull)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedObject <<< ActualBoolean)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedObject <<< ActualNumber)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedObject <<< ActualString)
    (invalid <<< h.onTypeMismatch pathSoFar ExpectedObject <<< ActualArray)
    (V <<< Right)
    json

decodeField :: forall e extra from a. Object Json -> String -> JsonDecoder e extra a -> JsonDecoder' e extra from a
decodeField obj field = decodeField' obj field do
  DecoderFn $ mkFn5 \pathSoFar _ (JsonErrorHandlers h) _ _ ->
    invalid $ h.onMissingField pathSoFar field

decodeField' :: forall e extra from a. Object Json -> String -> JsonDecoder' e extra from a -> JsonDecoder e extra a -> JsonDecoder' e extra from a
decodeField' obj field onMissingField decodeElem = case Object.lookup field obj of
  Nothing ->
    onMissingField
  Just a ->
    addOffset (AtKey field) a decodeElem

decodeVoid :: forall err extra. JsonDecoder err extra Void
decodeVoid = addTypeHint "Void" $ failWithUnrefinableValue "Decoding a value to Void is impossible"

decodeUnitFromNull :: forall err extra. JsonDecoder err extra Unit
decodeUnitFromNull = decodeJNull

decodeUnitFromAny :: forall e extra. JsonDecoder e extra Unit
decodeUnitFromAny = pure unit

decodeInt
  :: forall err extra
   . JsonDecoder err extra Int
decodeInt = addTypeHint "Int" Decoder.do
  n <- decodeNumber
  case Int.fromNumber n of
    Nothing ->
      failWithUnrefinableValue $ numToIntConversionFailure n
    Just i ->
      pure i

decodeChar
  :: forall err extra
   . JsonDecoder err extra Char
decodeChar = addTypeHint "Char" Decoder.do
  s <- decodeString
  case charAt 0 s of
    Nothing ->
      failWithUnrefinableValue $ stringToCharConversionFailure s
    Just c ->
      pure c

decodeNonEmptyString
  :: forall err extra
   . JsonDecoder err extra NonEmptyString
decodeNonEmptyString = addTypeHint "NonEmptyString" Decoder.do
  s <- decodeString
  case NonEmptyString.fromString s of
    Nothing ->
      failWithUnrefinableValue stringNotEmptyFailure
    Just nes ->
      pure nes

decodeArray
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Array a)
decodeArray decodeElem = Decoder.do
  arr <- decodeJArray
  forWithIndex arr \i j2 ->
    addOffset (AtIndex i) j2 decodeElem

decodeNonEmptyArray
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (NonEmptyArray a)
decodeNonEmptyArray decodeElem = addTypeHint "NonEmptyArray" Decoder.do
  arr <- decodeArray decodeElem
  case NEA.fromArray arr of
    Nothing -> failWithUnrefinableValue arrayNotEmptyFailure
    Just a -> pure a

decodeObject
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Object a)
decodeObject decodeElem = Decoder.do
  obj <- decodeJObject
  forWithIndex obj \field j2 ->
    addOffset (AtKey field) j2 decodeElem

decodeNullable
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Nullable a)
decodeNullable decodeA = addTypeHint "Nullable" Decoder.do
  altAccumulate (null <$ decodeJNull) (notNull <$> decodeA)

decodeIdentity
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Identity a)
decodeIdentity = addTypeHint "Identity" <<< coerce

decodeMaybeTagged
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Maybe a)
decodeMaybeTagged decodeElem = addTypeHint "Maybe" Decoder.do
  obj <- decodeJObject
  tag <- decodeField obj "tag" decodeString
  case tag of
    "Just" -> addCtorHint "Just" do
      Just <$> decodeField obj "value" decodeElem
    "Nothing" ->
      pure Nothing
    unknownTag ->
      failWithStructureError $ "Tag was not 'Just' or 'Nothing': " <> unknownTag

decodeMaybeNullable
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (Maybe a)
decodeMaybeNullable decodeElem = addTypeHint "Maybe" Decoder.do
  toMaybe <$> decodeNullable decodeElem

decodeEither
  :: forall err extra a b
   . JsonDecoder err extra a
  -> JsonDecoder err extra b
  -> JsonDecoder err extra (Either a b)
decodeEither decodeLeft decodeRight = addTypeHint "Either" Decoder.do
  obj <- decodeJObject
  tag <- decodeField obj "tag" decodeString
  case tag of
    "Left" -> addCtorHint "Left" do
      Left <$> decodeField obj "value" decodeLeft
    "Right" -> addCtorHint "Right" do
      Right <$> decodeField obj "value" decodeRight
    unknownTag ->
      failWithStructureError $ "Tag was not 'Left' or 'Right': " <> unknownTag

decodeTuple
  :: forall err extra a b
   . JsonDecoder err extra a
  -> JsonDecoder err extra b
  -> JsonDecoder err extra (Tuple a b)
decodeTuple decodeA decodeB = addTypeHint "Tuple" Decoder.do
  arr <- decodeJArray
  case arr of
    [ a, b ] -> do
      Tuple
        <$> (addSubtermHint 0 $ addOffset (AtIndex 0) a decodeA)
        <*> (addSubtermHint 1 $ addOffset (AtIndex 1) b decodeB)
    _ ->
      failWithStructureError $ "Expected array with 2 elements, but array had length of " <> show (Array.length arr)

decodeThese
  :: forall err extra a b
   . JsonDecoder err extra a
  -> JsonDecoder err extra b
  -> JsonDecoder err extra (These a b)
decodeThese decodeA decodeB = addTypeHint "These" Decoder.do
  obj <- decodeJObject
  tag <- decodeField obj "tag" decodeString
  case tag of
    "This" -> addCtorHint "This" do
      This <$> decodeField obj "value" decodeA
    "That" -> addCtorHint "That" do
      That <$> decodeField obj "value" decodeB
    "Both" -> addCtorHint "This" do
      Both
        <$> (addSubtermHint 0 $ decodeField obj "this" decodeA)
        <*> (addSubtermHint 1 $ decodeField obj "that" decodeB)
    unknownTag ->
      failWithStructureError $ "Tag was not 'This', 'That', or 'Both': " <> unknownTag

decodeNonEmpty
  :: forall err extra f a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (f a)
  -> JsonDecoder err extra (NonEmpty f a)
decodeNonEmpty decodeHead decodeTail = addTypeHint "NonEmpty" Decoder.do
  obj <- decodeJObject
  NonEmpty
    <$> (addSubtermHint 0 $ decodeField obj "head" decodeHead)
    <*> (addSubtermHint 0 $ decodeField obj "tail" decodeTail)

decodeList
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (List a)
decodeList decodeElem = addTypeHint "List" Decoder.do
  arr <- decodeJArray
  map List.fromFoldable $ forWithIndex arr \i a ->
    addOffset (AtIndex i) a decodeElem

decodeNonEmptyList
  :: forall err extra a
   . JsonDecoder err extra a
  -> JsonDecoder err extra (NonEmptyList a)
decodeNonEmptyList decodeA = addTypeHint "NonEmptyList" Decoder.do
  ls <- decodeList decodeA
  case ls of
    Nil ->
      failWithUnrefinableValue "Received empty list"
    Cons h t ->
      pure $ NonEmptyList $ NonEmpty h t

decodeMap
  :: forall err extra k v
   . Ord k
  => JsonDecoder err extra k
  -> JsonDecoder err extra v
  -> JsonDecoder err extra (Map k v)
decodeMap decodeKey decodeValue = addTypeHint "Map" Decoder.do
  arr <- decodeJArray
  map Map.fromFoldable $ forWithIndex arr \i a ->
    addOffset (AtIndex i) a Decoder.do
      obj <- decodeJObject
      Tuple
        <$> decodeField obj "key" decodeKey
        <*> decodeField obj "value" decodeValue

decodeSet
  :: forall err extra a
   . Ord a
  => JsonDecoder err extra a
  -> JsonDecoder err extra (Set a)
decodeSet decodeA = addTypeHint "Set" Decoder.do
  arr <- decodeJArray
  map Set.fromFoldable $ forWithIndex arr \i a ->
    addOffset (AtIndex i) a decodeA

decodeNonEmptySet
  :: forall err extra a
   . Ord a
  => JsonDecoder err extra a
  -> JsonDecoder err extra (NonEmptySet a)
decodeNonEmptySet decodeA = addTypeHint "NonEmptySet" Decoder.do
  s <- decodeSet decodeA
  case NonEmptySet.fromSet s of
    Nothing ->
      failWithUnrefinableValue "Received empty set"
    Just nes ->
      pure nes

decodeCodePoint
  :: forall err extra
   . JsonDecoder err extra CodePoint
decodeCodePoint = addTypeHint "CodePoint" Decoder.do
  s <- decodeString
  case codePointAt 0 s of
    Nothing ->
      failWithUnrefinableValue $ "Could not get code point from String: " <> s
    Just cp ->
      pure cp

decodeRecord
  :: forall err extra propsRl props decoderRl decoderRows outputRows
   . RowList.RowToList props propsRl
  => RowToList decoderRows decoderRl
  => InsertRequiredPropDecoders err extra propsRl { | props } {} { | decoderRows }
  => DecodeRowList err extra decoderRl { | decoderRows } { | outputRows }
  => { | props }
  -> JsonDecoder err extra { | outputRows }
decodeRecord props =
  decodeRecord' (buildRecordDecoder $ decodeRequiredProps props)

decodeRecord'
  :: forall err extra rl decoderRows outputRows
   . DecodeRowList err extra rl { | decoderRows } { | outputRows }
  => RLRecordDecoder err extra rl { | decoderRows }
  -> JsonDecoder err extra { | outputRows }
decodeRecord' propDecoders = decodeRecordPrim (decodeRowList propDecoders)

decodeRecordPrim
  :: forall err extra outputRows
   . (Object Json -> JsonDecoder err extra { | outputRows })
  -> JsonDecoder err extra { | outputRows }
decodeRecordPrim decoder = addTypeHint "Record" Decoder.do
  obj <- decodeJObject
  decoder obj

buildRecordDecoder
  :: forall err extra rl decoderRows
   . RowToList decoderRows rl
  => RLRecordDecoderBuilder err extra {} { | decoderRows }
  -> RLRecordDecoder err extra rl { | decoderRows }
buildRecordDecoder (RLRecordDecoderBuilder builder) =
  RLRecordDecoder $ buildFromScratch builder

decodeRequiredProp
  :: forall sym err extra a oldRows newRows
   . Row.Cons sym (PropDecoder err extra a) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder err extra a
  -> RLRecordDecoderBuilder err extra { | oldRows } { | newRows }
decodeRequiredProp _sym decoder =
  RLRecordDecoderBuilder (Builder.insert _sym (PropDecoder { onMissingField: failWithMissingField $ reflectSymbol _sym, decoder }))

decodeOptionalProp
  :: forall sym err extra a oldRows newRows
   . Row.Cons sym (PropDecoder err extra (Maybe a)) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder err extra a
  -> RLRecordDecoderBuilder err extra { | oldRows } { | newRows }
decodeOptionalProp _sym decoder =
  RLRecordDecoderBuilder (Builder.insert _sym (PropDecoder { onMissingField: pure Nothing, decoder: Just <$> decoder }))

decodeRequiredProps
  :: forall err extra propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders err extra propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder err extra { | oldRows } { | newRows })
decodeRequiredProps props =
  insertRequiredPropDecoders (RLRecordDecoder props :: RLRecordDecoder err extra propsRl { | props })

decodeOptionalProps
  :: forall err extra propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropDecoders err extra propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder err extra { | oldRows } { | newRows })
decodeOptionalProps props =
  insertOptionalPropDecoders (RLRecordDecoder props :: RLRecordDecoder err extra propsRl { | props })

newtype PropDecoder err extra a = PropDecoder
  { onMissingField :: JsonDecoder err extra a
  , decoder :: JsonDecoder err extra a
  }

class InsertRequiredPropDecoders :: Type -> Type -> RowList Type -> Type -> Type -> Type -> Constraint
class
  InsertRequiredPropDecoders err extra propsRl propsRec oldRec newRec
  | err extra propsRl -> propsRec oldRec newRec
  where
  insertRequiredPropDecoders
    :: RLRecordDecoder err extra propsRl propsRec
    -> RLRecordDecoderBuilder err extra oldRec newRec

instance InsertRequiredPropDecoders err extra RowList.Nil {} { | oldRows } { | oldRows } where
  insertRequiredPropDecoders _ = RLRecordDecoderBuilder identity

else instance
  ( Row.Cons sym (JsonDecoder err extra a) propsTail props
  , InsertRequiredPropDecoders err extra propsRlTail { | propsTail } { | oldRows } { | intermediateRows }
  , Row.Lacks sym intermediateRows
  , Row.Cons sym (PropDecoder err extra a) intermediateRows newRows
  , IsSymbol sym
  ) =>
  InsertRequiredPropDecoders
    err
    extra
    (RowList.Cons sym (JsonDecoder err extra a) propsRlTail)
    { | props }
    { | oldRows }
    { | newRows }
  where
  insertRequiredPropDecoders (RLRecordDecoder newDecoders) = do
    let
      _sym = Proxy :: Proxy sym

      tailDecoders :: { | propsTail }
      tailDecoders = unsafeCoerce newDecoders
      ((RLRecordDecoderBuilder intermediateDecoders) :: RLRecordDecoderBuilder err extra { | oldRows } { | intermediateRows }) =
        insertRequiredPropDecoders (RLRecordDecoder tailDecoders :: RLRecordDecoder err extra propsRlTail { | propsTail })
      propDecoder = PropDecoder { onMissingField: failWithMissingField $ reflectSymbol _sym, decoder: Record.get _sym newDecoders }
    RLRecordDecoderBuilder (intermediateDecoders >>> Builder.insert _sym propDecoder)

--
class InsertOptionalPropDecoders
  :: Type
  -> Type
  -> RowList Type
  -> Type
  -> Type
  -> Type
  -> Constraint
class
  InsertOptionalPropDecoders err extra propsRl propsRec oldRec newRec
  | err extra propsRl -> propsRec oldRec newRec where
  insertOptionalPropDecoders :: RLRecordDecoder err extra propsRl propsRec -> RLRecordDecoderBuilder err extra oldRec newRec

instance InsertOptionalPropDecoders err extra RowList.Nil {} { | oldRows } { | oldRows } where
  insertOptionalPropDecoders _ = RLRecordDecoderBuilder identity

else instance
  ( Row.Cons sym (JsonDecoder err extra a) propsTail props
  , InsertOptionalPropDecoders err extra propsRlTail { | propsTail } { | oldRows } { | intermediateRows }
  , Row.Lacks sym intermediateRows
  , Row.Cons sym (PropDecoder err extra (Maybe a)) intermediateRows newRows
  , IsSymbol sym
  ) =>
  InsertOptionalPropDecoders
    err
    extra
    (RowList.Cons sym (JsonDecoder err extra a) propsRlTail)
    { | props }
    { | oldRows }
    { | newRows }
  where
  insertOptionalPropDecoders (RLRecordDecoder newDecoders) = do
    let
      _sym = Proxy :: Proxy sym

      tailDecoders :: { | propsTail }
      tailDecoders = unsafeCoerce newDecoders
      ((RLRecordDecoderBuilder intermediateDecoders) :: RLRecordDecoderBuilder err extra { | oldRows } { | intermediateRows }) =
        insertOptionalPropDecoders (RLRecordDecoder tailDecoders :: RLRecordDecoder err extra propsRlTail { | propsTail })
      propDecoder = PropDecoder { onMissingField: pure Nothing, decoder: Just <$> Record.get _sym newDecoders }
    RLRecordDecoderBuilder (intermediateDecoders >>> Builder.insert _sym propDecoder)

newtype RLRecordDecoder :: Type -> Type -> RowList Type -> Type -> Type
newtype RLRecordDecoder err extra rowlist rec = RLRecordDecoder rec

newtype RLRecordDecoderBuilder :: Type -> Type -> Type -> Type -> Type
newtype RLRecordDecoderBuilder err extra fromRec toRec =
  RLRecordDecoderBuilder (Builder fromRec toRec)

instance Semigroupoid (RLRecordDecoderBuilder err extra) where
  compose (RLRecordDecoderBuilder l) (RLRecordDecoderBuilder r) = RLRecordDecoderBuilder $ l <<< r

instance Category (RLRecordDecoderBuilder err extra) where
  identity = RLRecordDecoderBuilder identity

-- | Decodes an `Object Json` into a `Record rows` and works whether fields are required or optional.
class DecodeRowList :: Type -> Type -> RowList Type -> Type -> Type -> Constraint
class DecodeRowList err extra rowList inputRec out | err rowList -> inputRec out where
  decodeRowList :: RLRecordDecoder err extra rowList inputRec -> Object Json -> JsonDecoder err extra out

instance DecodeRowList err extra RowList.Nil {} {} where
  decodeRowList _ _ = pure {}
else instance
  ( Row.Cons sym (PropDecoder err extra a) tail inputRows
  , DecodeRowList err extra tailList { | tail } { | intermediateRows }
  , Row.Lacks sym intermediateRows
  , Row.Cons sym a intermediateRows outRows
  , IsSymbol sym
  ) =>
  DecodeRowList err extra (RowList.Cons sym (PropDecoder err extra a) tailList) { | inputRows } { | outRows } where
  decodeRowList (RLRecordDecoder fieldDecoders) obj = ado
    tailRecord <- decodeRowList (RLRecordDecoder fieldDecodersTail :: RLRecordDecoder err extra tailList { | tail }) obj
    value <- decodeField' obj keyStr field.onMissingField field.decoder
    in Record.insert _sym value tailRecord
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    fieldDecodersTail :: { | tail }
    fieldDecodersTail = unsafeCoerce fieldDecoders

    (PropDecoder field :: PropDecoder err extra a) = Record.get _sym fieldDecoders
