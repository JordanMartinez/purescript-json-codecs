module Json.Unidirectional.Encode.Value
  ( encodeVoid
  , encodeNull
  , encodeUnitToNull
  , encodeBoolean
  , encodeNumber
  , encodeInt
  , encodeChar
  , encodeString
  , encodeNonEmptyString
  , encodeArray
  , encodeArrayPrim
  , encodeNonEmptyArray
  , encodeObject
  , encodeObjectPrim
  , encodeNullable
  , encodeIdentity
  , encodeMaybeTagged
  , encodeMaybeNullable
  , encodeEither
  , encodeTuple
  , encodeThese
  , encodeNonEmpty
  , encodeList
  , encodeNonEmptyList
  , encodeMap
  , encodeSet
  , encodeNonEmptySet
  , encodeCodePoint
  , RLRecordEncoder
  , RLRecordEncoderBuilder
  , encodeRecord
  , encodeRecord'
  , buildRecordEncoder
  , encodeRequiredProp
  , encodeOptionalProp
  , encodeRequiredProps
  , encodeOptionalProps
  , class InsertRequiredPropEncoders
  , insertRequiredPropEncoders
  , class InsertOptionalPropEncoders
  , insertOptionalPropEncoders
  , class EncodeRowList
  , encodeRowList
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet, toSet)
import Data.String (CodePoint)
import Data.String as SCP
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These, these)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

encodeVoid :: Void -> Json
encodeVoid = absurd

encodeNull :: Json
encodeNull = jsonNull

encodeUnitToNull :: Unit -> Json
encodeUnitToNull = const encodeNull

encodeBoolean :: Boolean -> Json
encodeBoolean = fromBoolean

encodeNumber :: Number -> Json
encodeNumber = fromNumber

encodeString :: String -> Json
encodeString = fromString

encodeInt :: Int -> Json
encodeInt = toNumber >>> encodeNumber

encodeChar :: Char -> Json
encodeChar = SCU.singleton >>> encodeString

encodeNonEmptyString :: NonEmptyString -> Json
encodeNonEmptyString (NonEmptyString s) = encodeString s

encodeArray :: forall a. (a -> Json) -> Array a -> Json
encodeArray encodeA = map encodeA >>> encodeArrayPrim

encodeArrayPrim :: Array Json -> Json
encodeArrayPrim = fromArray

encodeNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
encodeNonEmptyArray encodeA = NEA.toArray >>> encodeArray encodeA

encodeObject :: forall a. (a -> Json) -> Object a -> Json
encodeObject encodeA = map encodeA >>> encodeObjectPrim

encodeObjectPrim :: Object Json -> Json
encodeObjectPrim = fromObject

encodeNullable :: forall a. (a -> Json) -> Nullable a -> Json
encodeNullable encodeA = toMaybe >>> encodeMaybeNullable encodeA

encodeIdentity :: forall a. (a -> Json) -> Identity a -> Json
encodeIdentity encodeA (Identity a) = encodeA a

encodeMaybeTagged :: forall a. (a -> Json) -> Maybe a -> Json
encodeMaybeTagged encodeA =
  maybe (Object.singleton "tag" $ encodeString "Nothing") (tagged "Just" <<< encodeA)
    >>> encodeObjectPrim
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ encodeString tag, Tuple "value" j ]

encodeMaybeNullable :: forall a. (a -> Json) -> Maybe a -> Json
encodeMaybeNullable encodeA = maybe encodeNull encodeA

encodeEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
encodeEither encodeA encodeB =
  either (tagged "Left" <<< encodeA) (tagged "Right" <<< encodeB)
    >>> encodeObjectPrim
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ encodeString tag, Tuple "value" j ]

encodeTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
encodeTuple encodeA encodeB (Tuple a b) =
  encodeArrayPrim
    [ encodeA a
    , encodeB b
    ]

encodeThese :: forall a b. (a -> Json) -> (b -> Json) -> These a b -> Json
encodeThese encodeA encodeB =
  these
    (tagged "This" <<< encodeObjectPrim <<< Object.singleton "value" <<< encodeA)
    (tagged "That" <<< encodeObjectPrim <<< Object.singleton "value" <<< encodeB)
    ( \a b -> tagged "Both" $ encodeObjectPrim $ Object.fromFoldable
        [ Tuple "this" $ encodeA a
        , Tuple "that" $ encodeB b
        ]
    )
    >>> encodeObjectPrim
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ encodeString tag, Tuple "value" j ]

encodeNonEmpty :: forall f a. (a -> Json) -> (f a -> Json) -> NonEmpty f a -> Json
encodeNonEmpty encodeHead encodeTail (NonEmpty a fa) =
  encodeObjectPrim
    $ Object.fromFoldable
        [ Tuple "head" $ encodeHead a
        , Tuple "tail" $ encodeTail fa
        ]

encodeList :: forall a. (a -> Json) -> List a -> Json
encodeList encodeA = foldl (\arr -> Array.snoc arr <<< encodeA) [] >>> encodeArrayPrim

encodeNonEmptyList :: forall a. (a -> Json) -> (NonEmptyList a -> Json)
encodeNonEmptyList encodeA = foldl (\arr -> Array.snoc arr <<< encodeA) [] >>> encodeArrayPrim

encodeMap :: forall k v. (k -> Json) -> (v -> Json) -> Map k v -> Json
encodeMap encodeKey encodeValue =
  foldlWithIndex
    ( \k acc v -> Array.snoc acc
        $ encodeObjectPrim
        $ Object.fromFoldable
            [ Tuple "key" $ encodeKey k
            , Tuple "value" $ encodeValue v
            ]
    )
    []
    >>> encodeArrayPrim

encodeSet :: forall a. (a -> Json) -> Set a -> Json
encodeSet encodeA = foldl (\arr -> Array.snoc arr <<< encodeA) [] >>> encodeArrayPrim

encodeNonEmptySet :: forall a. (a -> Json) -> NonEmptySet a -> Json
encodeNonEmptySet encodeA = toSet >>> encodeSet encodeA

encodeCodePoint :: CodePoint -> Json
encodeCodePoint = SCP.singleton >>> encodeString

newtype RLRecordEncoder :: RowList.RowList Type -> Row Type -> Type
newtype RLRecordEncoder rowList rows = RLRecordEncoder { | rows }

newtype RLRecordEncoderBuilder :: RowList.RowList Type -> Row Type -> RowList.RowList Type -> Row Type -> Type
newtype RLRecordEncoderBuilder fromRl fromRows toRl toRows =
  RLRecordEncoderBuilder (Builder { | fromRows } { | toRows })

encodeRecord
  :: forall props propsRl newRl newRows inputRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropEncoders propsRl props RowList.Nil () newRl newRows
  => EncodeRowList newRl newRows inputRows
  => { | props }
  -> { | inputRows }
  -> Json
encodeRecord propEncoders input =
  encodeRecord' (buildRecordEncoder $ encodeRequiredProps propEncoders :: RLRecordEncoder newRl newRows) input

encodeRecord'
  :: forall encodeRows encodeRl inputRows
   . EncodeRowList encodeRl encodeRows inputRows
  => RLRecordEncoder encodeRl encodeRows
  -> { | inputRows }
  -> Json
encodeRecord' propEncoders = encodeObjectPrim <<< encodeRowList propEncoders

buildRecordEncoder
  :: forall encodeRows encodeRl
   . (RLRecordEncoderBuilder RowList.Nil () encodeRl encodeRows)
  -> RLRecordEncoder encodeRl encodeRows
buildRecordEncoder (RLRecordEncoderBuilder builder) =
  RLRecordEncoder (Builder.buildFromScratch builder)

encodeRequiredProp
  :: forall sym a oldRows oldRl newRows
   . Row.Cons sym (String -> a -> Object Json -> Object Json) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> (a -> Json)
  -> RLRecordEncoderBuilder oldRl oldRows (RowList.Cons sym (String -> a -> Object Json -> Object Json) oldRl) newRows
encodeRequiredProp _sym encoder =
  RLRecordEncoderBuilder (Builder.insert _sym (\str a obj -> Object.insert str (encoder a) obj))

encodeOptionalProp
  :: forall sym a oldRows oldRl newRows
   . Row.Cons sym (String -> Maybe a -> Object Json -> Object Json) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> (a -> Json)
  -> RLRecordEncoderBuilder oldRl oldRows (RowList.Cons sym (String -> Maybe a -> Object Json -> Object Json) oldRl) newRows
encodeOptionalProp _sym encoder =
  RLRecordEncoderBuilder (Builder.insert _sym (\str mbA obj -> maybe obj (\a' -> Object.insert str (encoder a') obj) mbA))

encodeRequiredProps
  :: forall props propsRl oldRl oldRows newRl newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropEncoders propsRl props oldRl oldRows newRl newRows
  => { | props }
  -> RLRecordEncoderBuilder oldRl oldRows newRl newRows
encodeRequiredProps props =
  insertRequiredPropEncoders (RLRecordEncoder props :: RLRecordEncoder propsRl props)

encodeOptionalProps
  :: forall props propsRl oldRl oldRows newRl newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropEncoders propsRl props oldRl oldRows newRl newRows
  => { | props }
  -> RLRecordEncoderBuilder oldRl oldRows newRl newRows
encodeOptionalProps props =
  insertOptionalPropEncoders (RLRecordEncoder props :: RLRecordEncoder propsRl props)

class InsertRequiredPropEncoders :: RowList.RowList Type -> Row Type -> RowList.RowList Type -> Row Type -> RowList.RowList Type -> Row Type -> Constraint
class InsertRequiredPropEncoders propsRl props oldRl oldRows newRl newRows | propsRl props oldRl oldRows -> newRl newRows where
  insertRequiredPropEncoders :: RLRecordEncoder propsRl props -> RLRecordEncoderBuilder oldRl oldRows newRl newRows

instance InsertRequiredPropEncoders RowList.Nil props oldRl oldRows oldRl oldRows where
  insertRequiredPropEncoders _ = RLRecordEncoderBuilder identity

else instance
  ( Row.Cons sym (a -> Json) propsTail props
  , InsertRequiredPropEncoders propsRlTail props oldRl oldRows tailRl tailRows
  , Row.Lacks sym tailRows
  , Row.Cons sym (String -> a -> Object Json -> Object Json) tailRows newRows
  , IsSymbol sym
  ) =>
  InsertRequiredPropEncoders
    (RowList.Cons sym (a -> Json) propsRlTail)
    props
    oldRl
    oldRows
    (RowList.Cons sym (String -> a -> Object Json -> Object Json) tailRl)
    newRows where
  insertRequiredPropEncoders (RLRecordEncoder newEncoders) = do
    let
      _sym = Proxy :: Proxy sym
      encoder = Record.get _sym newEncoders
      ((RLRecordEncoderBuilder tailEncoders) :: RLRecordEncoderBuilder oldRl oldRows tailRl tailRows) =
        insertRequiredPropEncoders (RLRecordEncoder newEncoders :: RLRecordEncoder propsRlTail props)
    RLRecordEncoderBuilder (tailEncoders >>> Builder.insert _sym (\str a obj -> Object.insert str (encoder a) obj))

---
class InsertOptionalPropEncoders :: RowList.RowList Type -> Row Type -> RowList.RowList Type -> Row Type -> RowList.RowList Type -> Row Type -> Constraint
class InsertOptionalPropEncoders propsRl props oldRl oldRows newRl newRows | propsRl props oldRl oldRows -> newRl newRows where
  insertOptionalPropEncoders :: RLRecordEncoder propsRl props -> RLRecordEncoderBuilder oldRl oldRows newRl newRows

instance InsertOptionalPropEncoders RowList.Nil props oldRl oldRows oldRl oldRows where
  insertOptionalPropEncoders _ = RLRecordEncoderBuilder identity

else instance
  ( Row.Cons sym (a -> Json) propsTail props
  , InsertOptionalPropEncoders propsRlTail props oldRl oldRows tailRl tailRows
  , Row.Lacks sym tailRows
  , Row.Cons sym (String -> Maybe a -> Object Json -> Object Json) tailRows newRows
  , IsSymbol sym
  ) =>
  InsertOptionalPropEncoders
    (RowList.Cons sym (a -> Json) propsRlTail)
    props
    oldRl
    oldRows
    (RowList.Cons sym (String -> Maybe a -> Object Json -> Object Json) tailRl)
    newRows where
  insertOptionalPropEncoders (RLRecordEncoder newEncoders) = do
    let
      _sym = Proxy :: Proxy sym
      encoder = Record.get _sym newEncoders
      ((RLRecordEncoderBuilder tailEncoders) :: RLRecordEncoderBuilder oldRl oldRows tailRl tailRows) =
        insertOptionalPropEncoders (RLRecordEncoder newEncoders :: RLRecordEncoder propsRlTail props)
    RLRecordEncoderBuilder (tailEncoders >>> Builder.insert _sym (\str mbA obj -> maybe obj (\a -> Object.insert str (encoder a) obj) mbA))

class EncodeRowList rowlist encodeRows inputRows | rowlist encodeRows -> inputRows where
  encodeRowList :: RLRecordEncoder rowlist encodeRows -> { | inputRows } -> Object Json

instance EncodeRowList RowList.Nil encodeRows inputRows where
  encodeRowList _ _ = Object.empty
else instance
  ( Row.Cons sym (String -> a -> Object Json -> Object Json) unused encodeRows
  , Row.Cons sym a inputTail inputRows
  , EncodeRowList tailList encodeRows inputRows
  , IsSymbol sym
  ) =>
  EncodeRowList (RowList.Cons sym (String -> a -> Object Json -> Object Json) tailList) encodeRows inputRows where
  encodeRowList (RLRecordEncoder encoders) inputRec =
    encoder keyStr value $ encodeRowList (RLRecordEncoder encoders :: RLRecordEncoder tailList encodeRows) inputRec
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    encoder :: String -> a -> Object Json -> Object Json
    encoder = Record.get _sym encoders

    value :: a
    value = Record.get _sym inputRec
