module Json.Encode.Class
  ( EncodeJsonInput
  , class EncodeJson
  , encodeJson
  , encodeJson'
  , ExistentialEncoder0
  , mkExistentialEncoder0
  , ExistentialEncoder1
  , mkExistentialEncoder1
  , ExistentialEncoder2
  , mkExistentialEncoder2
  , ExistentialEncoder3
  , mkExistentialEncoder3
  , RowListRecord
  , class EncodeRecordInput
  , encodeRecordInput
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
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
import Json.Types (K0(..), K1(..), K2(..), K3(..), Optional)
import Json.Unidirectional.Encode.Value (encodeArray, encodeBoolean, encodeChar, encodeCodePoint, encodeEither, encodeIdentity, encodeInt, encodeList, encodeMap, encodeMaybeTagged, encodeNonEmpty, encodeNonEmptyArray, encodeNonEmptyList, encodeNonEmptySet, encodeNonEmptyString, encodeNullable, encodeNumber, encodeObject, encodeObjectPrim, encodeSet, encodeString, encodeThese, encodeTuple, encodeUnitToNull, encodeVoid)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type EncodeJsonInput extra a =
  { value :: a
  , extra :: extra
  }

class EncodeJson extra a where
  encodeJson' :: EncodeJsonInput extra a -> Json

encodeJson :: forall a. EncodeJson Unit a => a -> Json
encodeJson = encodeJson' <<< { extra: unit, value: _ }

instance EncodeJson extra Void where
  encodeJson' = encodeVoid <<< _.value

instance EncodeJson extra Unit where
  encodeJson' = encodeUnitToNull <<< _.value

instance EncodeJson extra Boolean where
  encodeJson' = encodeBoolean <<< _.value

instance EncodeJson extra Int where
  encodeJson' = encodeInt <<< _.value

instance EncodeJson extra Char where
  encodeJson' = encodeChar <<< _.value

instance EncodeJson extra Number where
  encodeJson' = encodeNumber <<< _.value

instance EncodeJson extra String where
  encodeJson' = encodeString <<< _.value

instance EncodeJson extra NonEmptyString where
  encodeJson' = encodeNonEmptyString <<< _.value

instance EncodeJson extra a => EncodeJson extra (Array a) where
  encodeJson' { value, extra } = encodeArray (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (NonEmptyArray a) where
  encodeJson' { value, extra } = encodeNonEmptyArray (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (Object a) where
  encodeJson' { value, extra } = encodeObject (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (Nullable a) where
  encodeJson' { value, extra } = encodeNullable (encodeJson' <<< { extra, value: _ }) value

instance (Coercible a (Identity a), EncodeJson extra a) => EncodeJson extra (Identity a) where
  encodeJson' { value, extra } = encodeIdentity (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (Maybe a) where
  encodeJson' { value, extra } = encodeMaybeTagged (encodeJson' <<< { extra, value: _ }) value

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (Either l r) where
  encodeJson' { value, extra } = encodeEither (encodeJson' <<< { extra, value: _ }) (encodeJson' <<< { extra, value: _ }) value

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (Tuple l r) where
  encodeJson' { value, extra } = encodeTuple (encodeJson' <<< { extra, value: _ }) (encodeJson' <<< { extra, value: _ }) value

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (These l r) where
  encodeJson' { value, extra } = encodeThese (encodeJson' <<< { extra, value: _ }) (encodeJson' <<< { extra, value: _ }) value

instance (EncodeJson extra a, EncodeJson extra (f a)) => EncodeJson extra (NonEmpty f a) where
  encodeJson' { value, extra } = encodeNonEmpty (encodeJson' <<< { extra, value: _ }) (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (List a) where
  encodeJson' { value, extra } = encodeList (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra a => EncodeJson extra (NonEmptyList a) where
  encodeJson' { value, extra } = encodeNonEmptyList (encodeJson' <<< { extra, value: _ }) value

instance (Ord k, EncodeJson extra k, EncodeJson extra v) => EncodeJson extra (Map k v) where
  encodeJson' { value, extra } = encodeMap (encodeJson' <<< { extra, value: _ }) (encodeJson' <<< { extra, value: _ }) value

instance (Ord a, EncodeJson extra a) => EncodeJson extra (Set a) where
  encodeJson' { value, extra } = encodeSet (encodeJson' <<< { extra, value: _ }) value

instance (Ord a, EncodeJson extra a) => EncodeJson extra (NonEmptySet a) where
  encodeJson' { value, extra } = encodeNonEmptySet (encodeJson' <<< { extra, value: _ }) value

instance EncodeJson extra CodePoint where
  encodeJson' = encodeCodePoint <<< _.value

-- | Build a value via `mkExistentialEncoder0`.
foreign import data ExistentialEncoder0 :: Type -> Type

mkExistentialEncoder0 :: forall extra a. (EncodeJsonInput extra a -> Json) -> ExistentialEncoder0 a
mkExistentialEncoder0 = unsafeCoerce

unExistentialEncoder0 :: forall extra a. ExistentialEncoder0 a -> (EncodeJsonInput extra a -> Json)
unExistentialEncoder0 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder0 a) tail rows
  , IsSymbol sym
  ) =>
  EncodeJson extra (K0 sym a) where
  encodeJson' input = do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap input.extra

      f :: EncodeJsonInput extra a -> Json
      f = unExistentialEncoder0 $ Record.get (Proxy :: Proxy sym) localOverrides

      inputWithoutNewtype :: EncodeJsonInput extra a
      inputWithoutNewtype = coerce input

    f inputWithoutNewtype

-- | Build a value via `mkExistentialEncoder1`.
foreign import data ExistentialEncoder1 :: (Type -> Type) -> Type

mkExistentialEncoder1 :: forall extra f a. (EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra (f a) -> Json) -> ExistentialEncoder1 f
mkExistentialEncoder1 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder1 :: forall extra f a. ExistentialEncoder1 f -> ((EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra (f a) -> Json))
unExistentialEncoder1 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder1 f) tail rows
  , EncodeJson extra a
  , IsSymbol sym
  ) =>
  EncodeJson extra (K1 sym (f a)) where
  encodeJson' input = do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap input.extra

      buildEncoder :: (EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra (f a) -> Json)
      buildEncoder = unExistentialEncoder1 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJson'

      inputWithoutNewtype :: EncodeJsonInput extra (f a)
      inputWithoutNewtype = coerce input

    f inputWithoutNewtype

-- | Build a value via `mkExistentialEncoder2`.
foreign import data ExistentialEncoder2 :: (Type -> Type -> Type) -> Type

mkExistentialEncoder2 :: forall extra f a b. ((EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra (f a b) -> Json)) -> ExistentialEncoder2 f
mkExistentialEncoder2 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder2 :: forall extra f a b. ExistentialEncoder2 f -> ((EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra (f a b) -> Json))
unExistentialEncoder2 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder2 f) tail rows
  , EncodeJson extra a
  , EncodeJson extra b
  , IsSymbol sym
  ) =>
  EncodeJson extra (K2 sym (f a b)) where
  encodeJson' input = do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap input.extra

      buildEncoder :: (EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra (f a b) -> Json)
      buildEncoder = unExistentialEncoder2 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJson' encodeJson'

      inputWithoutNewtype :: EncodeJsonInput extra (f a b)
      inputWithoutNewtype = coerce input

    f inputWithoutNewtype

-- | Build a value via `mkExistentialEncoder3`.
foreign import data ExistentialEncoder3 :: (Type -> Type -> Type -> Type) -> Type

mkExistentialEncoder3 :: forall extra f a b c. ((EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra c -> Json) -> (EncodeJsonInput extra (f a b c) -> Json)) -> ExistentialEncoder3 f
mkExistentialEncoder3 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder3 :: forall extra f a b c. ExistentialEncoder3 f -> ((EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra c -> Json) -> (EncodeJsonInput extra (f a b c) -> Json))
unExistentialEncoder3 = unsafeCoerce

-- | Local overrides for types with kind `Type`. Implementation for types annotated with the `K3` type 
-- | are provided via the `extra` label in `JsonDecoderInput`. Value for `extra` must be a
-- | newtyped record. The `sym` in `K3 sym a` must be the label name witin that newtyped record
-- | that contains the function, `EncodeJsonInput extra a -> EncodeJsonInput extra b -> EncodeJsonInput extra c -> EncodeJsonInput extra (f a b c)`. The input
-- | arguments are the corresponding `DecodeJson` implementations for `a`, `b`, and `c`. Thus, one is only overriding
-- | the implementation for `f`, not `a`, `b`, and `c` as well.
instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder3 f) tail rows
  , EncodeJson extra a
  , EncodeJson extra b
  , EncodeJson extra c
  , IsSymbol sym
  ) =>
  EncodeJson extra (K3 sym (f a b c)) where
  encodeJson' input = do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap input.extra

      buildEncoder :: (EncodeJsonInput extra a -> Json) -> (EncodeJsonInput extra b -> Json) -> (EncodeJsonInput extra c -> Json) -> (EncodeJsonInput extra (f a b c) -> Json)
      buildEncoder = unExistentialEncoder3 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJson' encodeJson' encodeJson'

      inputWithoutNewtype :: EncodeJsonInput extra (f a b c)
      inputWithoutNewtype = coerce input

    f inputWithoutNewtype

instance
  ( RowToList.RowToList rows rl
  , EncodeRecordInput rl (EncodeJsonInput extra { | rows })
  ) =>
  EncodeJson extra { | rows } where
  encodeJson' input = encodeObjectPrim $ encodeRecordInput (RowListRecord input :: RowListRecord rl (EncodeJsonInput extra { | rows }))

newtype RowListRecord :: RowList.RowList Type -> Type -> Type
newtype RowListRecord rl rec = RowListRecord rec

class EncodeRecordInput :: RowList.RowList Type -> Type -> Constraint
class EncodeRecordInput rl rec | rl -> rec where
  encodeRecordInput :: RowListRecord rl rec -> Object Json

instance EncodeRecordInput RowList.Nil (EncodeJsonInput extra {}) where
  encodeRecordInput _ = Object.empty
else instance
  ( EncodeJson extra a
  , EncodeRecordInput tailList (EncodeJsonInput extra { | tailRows })
  , Row.Cons sym (Optional (Maybe a)) tailRows outRows
  , IsSymbol sym
  ) =>
  EncodeRecordInput (RowList.Cons sym (Optional (Maybe a)) tailList) (EncodeJsonInput extra { | outRows }) where
  encodeRecordInput (RowListRecord input) =
    encodeValue $ encodeRecordInput (RowListRecord tail :: RowListRecord tailList (EncodeJsonInput extra { | tailRows }))
    where
    encodeValue = case value of
      Nothing -> identity
      Just a -> Object.insert keyStr (encodeJson' { extra: input.extra, value: a })
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    tail :: EncodeJsonInput extra { | tailRows }
    tail = unsafeCoerce input

    value = unwrap $ Record.get _sym input.value
else instance
  ( EncodeJson extra a
  , EncodeRecordInput tailList (EncodeJsonInput extra { | tailRows })
  , Row.Cons sym a tailRows outRows
  , IsSymbol sym
  ) =>
  EncodeRecordInput (RowList.Cons sym a tailList) (EncodeJsonInput extra { | outRows }) where
  encodeRecordInput (RowListRecord input) =
    Object.insert keyStr (encodeJson' { extra: input.extra, value }) $ encodeRecordInput (RowListRecord tail :: RowListRecord tailList (EncodeJsonInput extra { | tailRows }))
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

    tail :: EncodeJsonInput extra { | tailRows }
    tail = unsafeCoerce input

    value :: a
    value = Record.get _sym input.value
