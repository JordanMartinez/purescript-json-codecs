module Json.Unidirectional.Decode.Value
  ( decodeVoid
  , decodeNullToUnit
  , decodeInt
  , decodeChar
  , decodeNonEmptyString
  , decodeArray
  , decodeNonEmptyArray
  , decodeObject
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
  , class RebuildRecord
  , rebuildRecord
  , module Exports
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Identity (Identity)
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
import Foreign.Object (Object)
import Json.Primitive.Decode (class IsDecodeJsonError, JsonDecoder(..), JsonOffset(..), addCtorHint, addSubtermHint, addTypeHint, alt, decodeField, decodeField', decodeString, failWithPath, onMissingField, onStructureError, onUnrefinableValue, withOffset)
import Json.Primitive.Decode (decodeBoolean, decodeNumber, decodeString, decodeNull, decodeArrayPrim, decodeIndex, decodeIndex', decodeObjectPrim, decodeField, decodeField') as Exports
import Json.Primitive.Decode as JPD
import Json.Primitive.Decode.Qualified as JPDQ
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder, buildFromScratch)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

decodeVoid :: forall err. IsDecodeJsonError err => JsonDecoder err Void
decodeVoid = addTypeHint "Void" $ failWithPath $ flip onUnrefinableValue "Decoding a value to Void is impossible"

decodeNullToUnit :: forall err. IsDecodeJsonError err => JsonDecoder err Unit
decodeNullToUnit = JPD.decodeNull

decodeInt
  :: forall err
   . IsDecodeJsonError err
  => JsonDecoder err Int
decodeInt = addTypeHint "Int" JPDQ.do
  n <- JPD.decodeNumber
  case Int.fromNumber n of
    Nothing ->
      failWithPath $ flip onUnrefinableValue $ "Could not convert Number to Int: " <> show n
    Just i ->
      pure i

decodeChar
  :: forall err
   . IsDecodeJsonError err
  => JsonDecoder err Char
decodeChar = addTypeHint "Char" JPDQ.do
  s <- JPD.decodeString
  case charAt 0 s of
    Nothing ->
      failWithPath $ flip onUnrefinableValue $ "Could not get char at index 0 in String: " <> s
    Just c ->
      pure c

decodeNonEmptyString
  :: forall err
   . IsDecodeJsonError err
  => JsonDecoder err NonEmptyString
decodeNonEmptyString = addTypeHint "NonEmptyString" JPDQ.do
  s <- JPD.decodeString
  case NonEmptyString.fromString s of
    Nothing ->
      failWithPath $ flip onUnrefinableValue $ "Received empty String"
    Just nes ->
      pure nes

decodeArray
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Array a)
decodeArray decodeElem = JPDQ.do
  arr <- JPD.decodeArrayPrim
  forWithIndex arr \i j2 ->
    withOffset (AtIndex i) j2 decodeElem

decodeNonEmptyArray
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (NonEmptyArray a)
decodeNonEmptyArray decodeElem = addTypeHint "NonEmptyArray" JPDQ.do
  arr <- decodeArray decodeElem
  case NEA.fromArray arr of
    Nothing -> failWithPath $ flip onUnrefinableValue $ "Received empty array"
    Just a -> pure a

decodeObject
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Object a)
decodeObject decodeElem = JPDQ.do
  obj <- JPD.decodeObjectPrim
  forWithIndex obj \field j2 ->
    withOffset (AtKey field) j2 decodeElem

decodeNullable
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Nullable a)
decodeNullable decodeA = addTypeHint "Nullable" JPDQ.do
  alt (null <$ JPD.decodeNull) (notNull <$> decodeA)

decodeIdentity
  :: forall err a
   . Coercible (JsonDecoder err a) (JsonDecoder err (Identity a))
  => IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Identity a)
decodeIdentity = addTypeHint "Identity" <<< coerce

decodeMaybeTagged
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Maybe a)
decodeMaybeTagged decodeElem = addTypeHint "Maybe" JPDQ.do
  obj <- JPD.decodeObjectPrim
  tag <- decodeField obj "tag" JPD.decodeString
  case tag of
    "Just" -> addCtorHint "Just" do
      Just <$> decodeField obj "value" decodeElem
    "Nothing" ->
      pure Nothing
    unknownTag ->
      failWithPath $ flip onStructureError $ "Tag was not 'Just' or 'Nothing': " <> unknownTag

decodeMaybeNullable
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (Maybe a)
decodeMaybeNullable decodeElem = addTypeHint "Maybe" JPDQ.do
  toMaybe <$> decodeNullable decodeElem

decodeEither
  :: forall err a b
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err b
  -> JsonDecoder err (Either a b)
decodeEither decodeLeft decodeRight = addTypeHint "Either" JPDQ.do
  obj <- JPD.decodeObjectPrim
  tag <- decodeField obj "tag" JPD.decodeString
  case tag of
    "Left" -> addCtorHint "Left" do
      Left <$> decodeField obj "value" decodeLeft
    "Right" -> addCtorHint "Right" do
      Right <$> decodeField obj "value" decodeRight
    unknownTag ->
      failWithPath $ flip onStructureError $ "Tag was not 'Left' or 'Right': " <> unknownTag

decodeTuple
  :: forall err a b
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err b
  -> JsonDecoder err (Tuple a b)
decodeTuple decodeA decodeB = addTypeHint "Tuple" JPDQ.do
  arr <- JPD.decodeArrayPrim
  case arr of
    [ a, b ] -> do
      Tuple
        <$> (addSubtermHint 0 $ withOffset (AtIndex 0) a decodeA)
        <*> (addSubtermHint 1 $ withOffset (AtIndex 1) b decodeB)
    _ ->
      failWithPath $ flip onStructureError $ "Expected array with 2 elements, but array had length of " <> show (Array.length arr)

decodeThese
  :: forall err a b
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err b
  -> JsonDecoder err (These a b)
decodeThese decodeA decodeB = addTypeHint "These" JPDQ.do
  obj <- JPD.decodeObjectPrim
  tag <- decodeField obj "tag" JPD.decodeString
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
      failWithPath $ flip onStructureError $ "Tag was not 'This', 'That', or 'Both': " <> unknownTag

decodeNonEmpty
  :: forall err f a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (f a)
  -> JsonDecoder err (NonEmpty f a)
decodeNonEmpty decodeHead decodeTail = addTypeHint "NonEmpty" JPDQ.do
  obj <- JPD.decodeObjectPrim
  NonEmpty
    <$> (addSubtermHint 0 $ decodeField obj "head" decodeHead)
    <*> (addSubtermHint 0 $ decodeField obj "tail" decodeTail)

decodeList
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (List a)
decodeList decodeElem = addTypeHint "List" JPDQ.do
  arr <- JPD.decodeArrayPrim
  map List.fromFoldable $ forWithIndex arr \i a ->
    withOffset (AtIndex i) a decodeElem

decodeNonEmptyList
  :: forall err a
   . IsDecodeJsonError err
  => JsonDecoder err a
  -> JsonDecoder err (NonEmptyList a)
decodeNonEmptyList decodeA = addTypeHint "NonEmptyList" JPDQ.do
  ls <- decodeList decodeA
  case ls of
    Nil ->
      failWithPath $ flip onUnrefinableValue "Received empty list"
    Cons h t ->
      pure $ NonEmptyList $ NonEmpty h t

decodeMap
  :: forall err k v
   . IsDecodeJsonError err
  => Ord k
  => JsonDecoder err k
  -> JsonDecoder err v
  -> JsonDecoder err (Map k v)
decodeMap decodeKey decodeValue = addTypeHint "Map" JPDQ.do
  arr <- JPD.decodeArrayPrim
  map Map.fromFoldable $ forWithIndex arr \i a ->
    withOffset (AtIndex i) a JPDQ.do
      obj <- JPD.decodeObjectPrim
      Tuple
        <$> decodeField obj "key" decodeKey
        <*> decodeField obj "value" decodeValue

decodeSet
  :: forall err a
   . IsDecodeJsonError err
  => Ord a
  => JsonDecoder err a
  -> JsonDecoder err (Set a)
decodeSet decodeA = addTypeHint "Set" JPDQ.do
  arr <- JPD.decodeArrayPrim
  map Set.fromFoldable $ forWithIndex arr \i a ->
    withOffset (AtIndex i) a decodeA

decodeNonEmptySet
  :: forall err a
   . IsDecodeJsonError err
  => Ord a
  => JsonDecoder err a
  -> JsonDecoder err (NonEmptySet a)
decodeNonEmptySet decodeA = addTypeHint "NonEmptySet" JPDQ.do
  s <- decodeSet decodeA
  case NonEmptySet.fromSet s of
    Nothing ->
      failWithPath $ flip onUnrefinableValue "Received empty set"
    Just nes ->
      pure nes

decodeCodePoint
  :: forall err
   . IsDecodeJsonError err
  => JsonDecoder err CodePoint
decodeCodePoint = addTypeHint "CodePoint" JPDQ.do
  s <- decodeString
  case codePointAt 0 s of
    Nothing ->
      failWithPath $ flip onUnrefinableValue $ "Could not get code point from String: " <> s
    Just cp ->
      pure cp

decodeRecord
  :: forall err propsRl props decoderRl decoderRows tuples outputRows
   . IsDecodeJsonError err
  => RowList.RowToList props propsRl
  => RowToList decoderRows decoderRl
  => InsertRequiredPropDecoders err propsRl { | props } {} { | decoderRows }
  => DecodeRowList err decoderRl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => { | props }
  -> JsonDecoder err { | outputRows }
decodeRecord props =
  decodeRecord' (buildRecordDecoder $ decodeRequiredProps props)

decodeRecord'
  :: forall err rl decoderRows outputRows tuples
   . IsDecodeJsonError err
  => DecodeRowList err rl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => RLRecordDecoder err rl { | decoderRows }
  -> JsonDecoder err { | outputRows }
decodeRecord' propDecoders = decodeRecordPrim (decodeRowList propDecoders)

decodeRecordPrim
  :: forall err outputRows tuples
   . IsDecodeJsonError err
  => RebuildRecord tuples {} { | outputRows }
  => (Object Json -> JsonDecoder err tuples)
  -> JsonDecoder err { | outputRows }
decodeRecordPrim decoder = addTypeHint "Record" JPDQ.do
  obj <- JPD.decodeObjectPrim
  buildFromScratch <<< rebuildRecord <$> decoder obj

buildRecordDecoder
  :: forall err rl decoderRows
   . RowToList decoderRows rl
  => RLRecordDecoderBuilder err {} { | decoderRows }
  -> RLRecordDecoder err rl { | decoderRows }
buildRecordDecoder (RLRecordDecoderBuilder builder) =
  RLRecordDecoder $ buildFromScratch builder

decodeRequiredProp
  :: forall sym err a oldRows newRows
   . Row.Cons sym (PropDecoder err a) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => IsDecodeJsonError err
  => Proxy sym
  -> JsonDecoder err a
  -> RLRecordDecoderBuilder err { | oldRows } { | newRows }
decodeRequiredProp _sym decoder =
  RLRecordDecoderBuilder (Builder.insert _sym (PropDecoder { onMissingField: failWithPath \p -> onMissingField p $ reflectSymbol _sym, decoder }))

decodeOptionalProp
  :: forall sym err a oldRows newRows
   . Row.Cons sym (PropDecoder err (Maybe a)) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => IsDecodeJsonError err
  => Proxy sym
  -> JsonDecoder err a
  -> RLRecordDecoderBuilder err { | oldRows } { | newRows }
decodeOptionalProp _sym decoder =
  RLRecordDecoderBuilder (Builder.insert _sym (PropDecoder { onMissingField: pure Nothing, decoder: Just <$> decoder }))

decodeRequiredProps
  :: forall err propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders err propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder err { | oldRows } { | newRows })
decodeRequiredProps props =
  insertRequiredPropDecoders (RLRecordDecoder props :: RLRecordDecoder err propsRl { | props })

decodeOptionalProps
  :: forall err propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropDecoders err propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder err { | oldRows } { | newRows })
decodeOptionalProps props =
  insertOptionalPropDecoders (RLRecordDecoder props :: RLRecordDecoder err propsRl { | props })

newtype PropDecoder err a = PropDecoder
  { onMissingField :: JsonDecoder err a
  , decoder :: JsonDecoder err a
  }

class InsertRequiredPropDecoders :: Type -> RowList Type -> Type -> Type -> Type -> Constraint
class
  InsertRequiredPropDecoders err propsRl propsRec oldRec newRec
  | err propsRl -> propsRec oldRec newRec
  where
  insertRequiredPropDecoders
    :: RLRecordDecoder err propsRl propsRec
    -> RLRecordDecoderBuilder err oldRec newRec

instance InsertRequiredPropDecoders err RowList.Nil {} { | oldRows } { | oldRows } where
  insertRequiredPropDecoders _ = RLRecordDecoderBuilder identity

else instance
  ( Row.Cons sym (JsonDecoder err a) propsTail props
  , InsertRequiredPropDecoders err propsRlTail { | propsTail } { | oldRows } { | intermediateRows }
  , Row.Lacks sym intermediateRows
  , Row.Cons sym (PropDecoder err a) intermediateRows newRows
  , IsSymbol sym
  , IsDecodeJsonError err
  ) =>
  InsertRequiredPropDecoders
    err
    (RowList.Cons sym (JsonDecoder err a) propsRlTail)
    { | props }
    { | oldRows }
    { | newRows }
  where
  insertRequiredPropDecoders (RLRecordDecoder newDecoders) = do
    let
      _sym = Proxy :: Proxy sym

      tailDecoders :: { | propsTail }
      tailDecoders = unsafeCoerce newDecoders
      ((RLRecordDecoderBuilder intermediateDecoders) :: RLRecordDecoderBuilder err { | oldRows } { | intermediateRows }) =
        insertRequiredPropDecoders (RLRecordDecoder tailDecoders :: RLRecordDecoder err propsRlTail { | propsTail })
      propDecoder = PropDecoder { onMissingField: failWithPath \p -> onMissingField p $ reflectSymbol _sym, decoder: Record.get _sym newDecoders }
    RLRecordDecoderBuilder (intermediateDecoders >>> Builder.insert _sym propDecoder)

--
class InsertOptionalPropDecoders
  :: Type
  -> RowList Type
  -> Type
  -> Type
  -> Type
  -> Constraint
class
  InsertOptionalPropDecoders err propsRl propsRec oldRec newRec
  | err propsRl -> propsRec oldRec newRec where
  insertOptionalPropDecoders :: RLRecordDecoder err propsRl propsRec -> RLRecordDecoderBuilder err oldRec newRec

instance InsertOptionalPropDecoders err RowList.Nil {} { | oldRows } { | oldRows } where
  insertOptionalPropDecoders _ = RLRecordDecoderBuilder identity

else instance
  ( Row.Cons sym (JsonDecoder err a) propsTail props
  , InsertOptionalPropDecoders err propsRlTail { | propsTail } { | oldRows } { | intermediateRows }
  , Row.Lacks sym intermediateRows
  , Row.Cons sym (PropDecoder err (Maybe a)) intermediateRows newRows
  , IsSymbol sym
  , IsDecodeJsonError err
  ) =>
  InsertOptionalPropDecoders
    err
    (RowList.Cons sym (JsonDecoder err a) propsRlTail)
    { | props }
    { | oldRows }
    { | newRows }
  where
  insertOptionalPropDecoders (RLRecordDecoder newDecoders) = do
    let
      _sym = Proxy :: Proxy sym

      tailDecoders :: { | propsTail }
      tailDecoders = unsafeCoerce newDecoders
      ((RLRecordDecoderBuilder intermediateDecoders) :: RLRecordDecoderBuilder err { | oldRows } { | intermediateRows }) =
        insertOptionalPropDecoders (RLRecordDecoder tailDecoders :: RLRecordDecoder err propsRlTail { | propsTail })
      propDecoder = PropDecoder { onMissingField: pure Nothing, decoder: Just <$> Record.get _sym newDecoders }
    RLRecordDecoderBuilder (intermediateDecoders >>> Builder.insert _sym propDecoder)

newtype RLRecordDecoder :: Type -> RowList Type -> Type -> Type
newtype RLRecordDecoder err rowlist rec = RLRecordDecoder rec

newtype RLRecordDecoderBuilder :: Type -> Type -> Type -> Type
newtype RLRecordDecoderBuilder err fromRec toRec =
  RLRecordDecoderBuilder (Builder fromRec toRec)

class DecodeRowList :: Type -> RowList Type -> Type -> Type -> Constraint
class DecodeRowList err rowList inputRec out | err rowList -> inputRec out where
  decodeRowList :: IsDecodeJsonError err => RLRecordDecoder err rowList inputRec -> Object Json -> JsonDecoder err out

-- I think I need to revert back to using `Validation`
-- and then define a special function that allows
-- a `These`-like accumulation for `Alt` specifically.
instance DecodeRowList err RowList.Nil {} Unit where
  decodeRowList _ _ = pure unit
else instance
  ( Row.Cons sym (PropDecoder err a) tail inputRows
  , DecodeRowList err tailList { | tail } out
  , IsSymbol sym
  ) =>
  DecodeRowList err (RowList.Cons sym (PropDecoder err a) tailList) { | inputRows } (Tuple (Tuple (Proxy sym) a) out) where
  decodeRowList (RLRecordDecoder fieldDecoders) obj = ado
    tailRecord <- decodeRowList (RLRecordDecoder fieldDecodersTail :: RLRecordDecoder err tailList { | tail }) obj
    label <- Tuple _sym <$> decodeField' obj keyStr field.onMissingField field.decoder
    in Tuple label tailRecord
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    fieldDecodersTail :: { | tail }
    fieldDecodersTail = unsafeCoerce fieldDecoders

    (PropDecoder field :: PropDecoder err a) = Record.get _sym fieldDecoders

class RebuildRecord a from to | a -> from to where
  rebuildRecord :: a -> Builder from to

instance RebuildRecord Unit {} {} where
  rebuildRecord _ = identity
else instance
  ( RebuildRecord tail { | beforeRows } { | middleRows }
  , Row.Cons sym a middleRows afterRows
  , Row.Lacks sym middleRows
  , IsSymbol sym
  ) =>
  RebuildRecord (Tuple (Tuple (Proxy sym) a) tail) { | beforeRows } { | afterRows } where
  rebuildRecord (Tuple (Tuple _sym a) tail) =
    rebuildRecord tail >>> Builder.insert _sym a