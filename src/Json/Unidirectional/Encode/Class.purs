module Json.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (CodePoint)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Foreign.Object as Object
import Json.Unidirectional.Encode.Value (encodeArray, encodeBoolean, encodeChar, encodeCodePoint, encodeEither, encodeIdentity, encodeInt, encodeList, encodeMap, encodeMaybeTagged, encodeNonEmpty, encodeNonEmptyArray, encodeNonEmptyList, encodeNonEmptySet, encodeNonEmptyString, encodeNullable, encodeNumber, encodeObject, encodeObjectPrim, encodeSet, encodeString, encodeThese, encodeTuple, encodeUnitToNull, encodeVoid)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class EncodeJson a where
  encodeJson :: a -> Json

instance EncodeJson Void where
  encodeJson = encodeVoid

instance EncodeJson Unit where
  encodeJson = encodeUnitToNull

instance EncodeJson Boolean where
  encodeJson = encodeBoolean

instance EncodeJson Int where
  encodeJson = encodeInt

instance EncodeJson Char where
  encodeJson = encodeChar

instance EncodeJson Number where
  encodeJson = encodeNumber

instance EncodeJson String where
  encodeJson = encodeString

instance EncodeJson NonEmptyString where
  encodeJson = encodeNonEmptyString

instance EncodeJson a => EncodeJson (Array a) where
  encodeJson = encodeArray encodeJson

instance EncodeJson a => EncodeJson (NonEmptyArray a) where
  encodeJson = encodeNonEmptyArray encodeJson

instance EncodeJson a => EncodeJson (Object a) where
  encodeJson = encodeObject encodeJson

instance EncodeJson a => EncodeJson (Nullable a) where
  encodeJson = encodeNullable encodeJson

instance (Coercible a (Identity a), EncodeJson a) => EncodeJson (Identity a) where
  encodeJson = encodeIdentity encodeJson

instance EncodeJson a => EncodeJson (Maybe a) where
  encodeJson = encodeMaybeTagged encodeJson

instance (EncodeJson l, EncodeJson r) => EncodeJson (Either l r) where
  encodeJson = encodeEither encodeJson encodeJson

instance (EncodeJson l, EncodeJson r) => EncodeJson (Tuple l r) where
  encodeJson = encodeTuple encodeJson encodeJson

instance (EncodeJson l, EncodeJson r) => EncodeJson (These l r) where
  encodeJson = encodeThese encodeJson encodeJson

instance (EncodeJson a, EncodeJson (f a)) => EncodeJson (NonEmpty f a) where
  encodeJson = encodeNonEmpty encodeJson encodeJson

instance EncodeJson a => EncodeJson (List a) where
  encodeJson = encodeList encodeJson

instance EncodeJson a => EncodeJson (NonEmptyList a) where
  encodeJson = encodeNonEmptyList encodeJson

instance (Ord k, EncodeJson k, EncodeJson v) => EncodeJson (Map k v) where
  encodeJson = encodeMap encodeJson encodeJson

instance (Ord a, EncodeJson a) => EncodeJson (Set a) where
  encodeJson = encodeSet encodeJson

instance (Ord a, EncodeJson a) => EncodeJson (NonEmptySet a) where
  encodeJson = encodeNonEmptySet encodeJson

instance EncodeJson CodePoint where
  encodeJson = encodeCodePoint

instance
  ( RowToList.RowToList rows rl
  , EncodeRecordInput rl { | rows }
  ) =>
  EncodeJson { | rows } where
  encodeJson input = encodeObjectPrim $ encodeRecordInput (RowListRecord input :: RowListRecord rl { | rows })

newtype RowListRecord :: RowList.RowList Type -> Type -> Type
newtype RowListRecord rl rec = RowListRecord rec

class EncodeRecordInput :: RowList.RowList Type -> Type -> Constraint
class EncodeRecordInput rl rec | rl -> rec where
  encodeRecordInput :: RowListRecord rl rec -> Object Json

instance EncodeRecordInput RowList.Nil {} where
  encodeRecordInput _ = Object.empty
else instance
  ( EncodeJson a
  , EncodeRecordInput tailList { | tailRows }
  , Row.Cons sym a tailRows outRows
  , IsSymbol sym
  ) =>
  EncodeRecordInput (RowList.Cons sym a tailList) { | outRows } where
  encodeRecordInput (RowListRecord input) =
    Object.insert keyStr (encodeJson value) $ encodeRecordInput (RowListRecord tail :: RowListRecord tailList { | tailRows })
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    tail :: { | tailRows }
    tail = unsafeCoerce input

    value = Record.get _sym input
