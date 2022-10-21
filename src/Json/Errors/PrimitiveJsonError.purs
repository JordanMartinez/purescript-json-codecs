module Json.Errors.PrimitiveJsonError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (bifoldMap)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List.Types (List, NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (CodePoint)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Symbol (class IsSymbol)
import Data.These (These(..))
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Json.Errors.Tree (TreeError(..))
import Json.Primitive.Decode (class IsDecodeJsonError, ActualJsonType, ExpectedJsonType, JsonDecoder, JsonOffset, TypeHint, failWithPath, onError, printActualJsonType, printExpectedJsonType, printJsonOffsetPath, printTypeHint)
import Json.Unidirectional.Decode.Value (class DecodeRowList, class InsertOptionalPropDecoders, class InsertRequiredPropDecoders, class RebuildRecord, PropDecoder, RLRecordDecoder, RLRecordDecoderBuilder)
import Json.Unidirectional.Decode.Value as JUDV
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Type.Proxy (Proxy)

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

instance IsDecodeJsonError PrimitiveJsonError where
  onTypeMismatch :: Array JsonOffset -> ExpectedJsonType -> ActualJsonType -> PrimitiveJsonError
  onTypeMismatch path exp = PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< TypeMismatch exp

  onMissingField :: Array JsonOffset -> String -> PrimitiveJsonError
  onMissingField path = PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< MissingField

  onMissingIndex :: Array JsonOffset -> Int -> PrimitiveJsonError
  onMissingIndex path = PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< MissingIndex

  onUnrefinableValue :: Array JsonOffset -> String -> PrimitiveJsonError
  onUnrefinableValue path = PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< UnrefinableValue

  onStructureError :: Array JsonOffset -> String -> PrimitiveJsonError
  onStructureError path = PrimitiveJsonError <<< TreeError <<< Right <<< { path, error: _ } <<< StructureError

  withHint :: forall a. TypeHint -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
  withHint hint = onError \path -> over PrimitiveJsonError case _ of
    TreeError (Left (That x)) -> TreeError $ Left $ Both { path, hint } x
    x -> TreeError $ Left $ Both { path, hint } $ NEA.singleton x

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

failWithLeaf :: forall a. JsonLeafError -> JsonDecoder PrimitiveJsonError a
failWithLeaf leaf = failWithPath \path ->
  PrimitiveJsonError $ TreeError $ Right { path, error: leaf }

decodeVoid :: JsonDecoder PrimitiveJsonError Void
decodeVoid = JUDV.decodeVoid

decodeNullToUnit :: JsonDecoder PrimitiveJsonError Unit
decodeNullToUnit = JUDV.decodeNullToUnit

decodeNull :: JsonDecoder PrimitiveJsonError Unit
decodeNull = JUDV.decodeNull

decodeBoolean :: JsonDecoder PrimitiveJsonError Boolean
decodeBoolean = JUDV.decodeBoolean

decodeNumber :: JsonDecoder PrimitiveJsonError Number
decodeNumber = JUDV.decodeNumber

decodeString :: JsonDecoder PrimitiveJsonError String
decodeString = JUDV.decodeString

decodeArrayPrim :: JsonDecoder PrimitiveJsonError (Array Json)
decodeArrayPrim = JUDV.decodeArrayPrim

decodeIndex :: forall a. Array Json -> Int -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
decodeIndex = JUDV.decodeIndex

decodeIndex' :: forall a. Array Json -> Int -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
decodeIndex' = JUDV.decodeIndex'

decodeObjectPrim :: JsonDecoder PrimitiveJsonError (Object Json)
decodeObjectPrim = JUDV.decodeObjectPrim

decodeField :: forall a. Object Json -> String -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
decodeField = JUDV.decodeField

decodeField' :: forall a. Object Json -> String -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a -> JsonDecoder PrimitiveJsonError a
decodeField' = JUDV.decodeField'

decodeInt :: JsonDecoder PrimitiveJsonError Int
decodeInt = JUDV.decodeInt

decodeChar :: JsonDecoder PrimitiveJsonError Char
decodeChar = JUDV.decodeChar

decodeNonEmptyString :: JsonDecoder PrimitiveJsonError NonEmptyString
decodeNonEmptyString = JUDV.decodeNonEmptyString

decodeArray
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Array a)
decodeArray = JUDV.decodeArray

decodeNonEmptyArray
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (NonEmptyArray a)
decodeNonEmptyArray = JUDV.decodeNonEmptyArray

decodeObject
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Object a)
decodeObject = JUDV.decodeObject

decodeNullable
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Nullable a)
decodeNullable = JUDV.decodeNullable

decodeIdentity
  :: forall a
   . Coercible (JsonDecoder PrimitiveJsonError a) (JsonDecoder PrimitiveJsonError (Identity a))
  => JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Identity a)
decodeIdentity = JUDV.decodeIdentity

decodeMaybeTagged
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Maybe a)
decodeMaybeTagged = JUDV.decodeMaybeTagged

decodeMaybeNullable
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Maybe a)
decodeMaybeNullable = JUDV.decodeMaybeNullable

decodeEither
  :: forall a b
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError b
  -> JsonDecoder PrimitiveJsonError (Either a b)
decodeEither = JUDV.decodeEither

decodeTuple
  :: forall a b
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError b
  -> JsonDecoder PrimitiveJsonError (Tuple a b)
decodeTuple = JUDV.decodeTuple

decodeThese
  :: forall a b
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError b
  -> JsonDecoder PrimitiveJsonError (These a b)
decodeThese = JUDV.decodeThese

decodeNonEmpty
  :: forall f a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (f a)
  -> JsonDecoder PrimitiveJsonError (NonEmpty f a)
decodeNonEmpty = JUDV.decodeNonEmpty

decodeList
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (List a)
decodeList = JUDV.decodeList

decodeNonEmptyList
  :: forall a
   . JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (NonEmptyList a)
decodeNonEmptyList = JUDV.decodeNonEmptyList

decodeMap
  :: forall k v
   . Ord k
  => JsonDecoder PrimitiveJsonError k
  -> JsonDecoder PrimitiveJsonError v
  -> JsonDecoder PrimitiveJsonError (Map k v)
decodeMap = JUDV.decodeMap

decodeSet
  :: forall a
   . Ord a
  => JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (Set a)
decodeSet = JUDV.decodeSet

decodeNonEmptySet
  :: forall a
   . Ord a
  => JsonDecoder PrimitiveJsonError a
  -> JsonDecoder PrimitiveJsonError (NonEmptySet a)
decodeNonEmptySet = JUDV.decodeNonEmptySet

decodeCodePoint :: JsonDecoder PrimitiveJsonError CodePoint
decodeCodePoint = JUDV.decodeCodePoint

decodeRecord
  :: forall propsRl props decoderRows decoderRl tuples outputRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders PrimitiveJsonError propsRl { | props } {} { | decoderRows }
  => RowList.RowToList decoderRows decoderRl
  => DecodeRowList PrimitiveJsonError decoderRl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => { | props }
  -> JsonDecoder PrimitiveJsonError { | outputRows }
decodeRecord = JUDV.decodeRecord

decodeRecord'
  :: forall rl decoderRows tuples outputRows
   . DecodeRowList PrimitiveJsonError rl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => RLRecordDecoder PrimitiveJsonError rl { | decoderRows }
  -> JsonDecoder PrimitiveJsonError { | outputRows }
decodeRecord' = JUDV.decodeRecord'

buildRecordDecoder
  :: forall rl decoderRows
   . RowList.RowToList decoderRows rl
  => RLRecordDecoderBuilder PrimitiveJsonError {} { | decoderRows }
  -> RLRecordDecoder PrimitiveJsonError rl { | decoderRows }
buildRecordDecoder = JUDV.buildRecordDecoder

decodeRequiredProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder PrimitiveJsonError a) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder PrimitiveJsonError a
  -> RLRecordDecoderBuilder PrimitiveJsonError { | oldRows } { | newRows }
decodeRequiredProp = JUDV.decodeRequiredProp

decodeOptionalProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder PrimitiveJsonError (Maybe a)) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder PrimitiveJsonError a
  -> RLRecordDecoderBuilder PrimitiveJsonError { | oldRows } { | newRows }
decodeOptionalProp = JUDV.decodeOptionalProp

decodeRequiredProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders PrimitiveJsonError propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder PrimitiveJsonError { | oldRows } { | newRows })
decodeRequiredProps = JUDV.decodeRequiredProps

decodeOptionalProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropDecoders PrimitiveJsonError propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder PrimitiveJsonError { | oldRows } { | newRows })
decodeOptionalProps = JUDV.decodeOptionalProps
