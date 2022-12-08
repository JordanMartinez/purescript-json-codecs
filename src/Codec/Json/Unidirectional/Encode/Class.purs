module Codec.Json.Unidirectional.Encode.Class
  ( class EncodeJson
  , encodeJson
  , encodeJson'
  , encodeJsonFn
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
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
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
import Codec.Json.Newtypes (K0(..), K1(..), K2(..), K3(..), Optional)
import Codec.Json.Unidirectional.Encode.Value (encodeArray, encodeBoolean, encodeChar, encodeCodePoint, encodeEither, encodeIdentity, encodeInt, encodeList, encodeMap, encodeMaybeTagged, encodeNonEmpty, encodeNonEmptyArray, encodeNonEmptyList, encodeNonEmptySet, encodeNonEmptyString, encodeNullable, encodeNumber, encodeObject, encodeJObject, encodeSet, encodeString, encodeThese, encodeTuple, encodeUnitToNull, encodeVoid)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class EncodeJson extra a where
  encodeJsonFn :: Fn2 extra a Json

encodeJson :: forall a. EncodeJson Unit a => a -> Json
encodeJson = encodeJson' unit

encodeJson' :: forall extra a. EncodeJson extra a => extra -> a -> Json
encodeJson' = runFn2 encodeJsonFn

instance EncodeJson extra Json where
  encodeJsonFn = mkFn2 \_ -> identity

instance EncodeJson extra Void where
  encodeJsonFn = mkFn2 \_ -> encodeVoid

instance EncodeJson extra Unit where
  encodeJsonFn = mkFn2 \_ -> encodeUnitToNull

instance EncodeJson extra Boolean where
  encodeJsonFn = mkFn2 \_ -> encodeBoolean

instance EncodeJson extra Int where
  encodeJsonFn = mkFn2 \_ -> encodeInt

instance EncodeJson extra Char where
  encodeJsonFn = mkFn2 \_ -> encodeChar

instance EncodeJson extra Number where
  encodeJsonFn = mkFn2 \_ -> encodeNumber

instance EncodeJson extra String where
  encodeJsonFn = mkFn2 \_ -> encodeString

instance EncodeJson extra NonEmptyString where
  encodeJsonFn = mkFn2 \_ -> encodeNonEmptyString

instance EncodeJson extra a => EncodeJson extra (Array a) where
  encodeJsonFn = mkFn2 \extra -> encodeArray (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (NonEmptyArray a) where
  encodeJsonFn = mkFn2 \extra -> encodeNonEmptyArray (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (Object a) where
  encodeJsonFn = mkFn2 \extra -> encodeObject (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (Nullable a) where
  encodeJsonFn = mkFn2 \extra -> encodeNullable (runFn2 encodeJsonFn extra)

instance (Coercible a (Identity a), EncodeJson extra a) => EncodeJson extra (Identity a) where
  encodeJsonFn = mkFn2 \extra -> encodeIdentity (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (Maybe a) where
  encodeJsonFn = mkFn2 \extra -> encodeMaybeTagged (runFn2 encodeJsonFn extra)

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (Either l r) where
  encodeJsonFn = mkFn2 \extra -> encodeEither (runFn2 encodeJsonFn extra) (runFn2 encodeJsonFn extra)

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (Tuple l r) where
  encodeJsonFn = mkFn2 \extra -> encodeTuple (runFn2 encodeJsonFn extra) (runFn2 encodeJsonFn extra)

instance (EncodeJson extra l, EncodeJson extra r) => EncodeJson extra (These l r) where
  encodeJsonFn = mkFn2 \extra -> encodeThese (runFn2 encodeJsonFn extra) (runFn2 encodeJsonFn extra)

instance (EncodeJson extra a, EncodeJson extra (f a)) => EncodeJson extra (NonEmpty f a) where
  encodeJsonFn = mkFn2 \extra -> encodeNonEmpty (runFn2 encodeJsonFn extra) (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (List a) where
  encodeJsonFn = mkFn2 \extra -> encodeList (runFn2 encodeJsonFn extra)

instance EncodeJson extra a => EncodeJson extra (NonEmptyList a) where
  encodeJsonFn = mkFn2 \extra -> encodeNonEmptyList (runFn2 encodeJsonFn extra)

instance (Ord k, EncodeJson extra k, EncodeJson extra v) => EncodeJson extra (Map k v) where
  encodeJsonFn = mkFn2 \extra -> encodeMap (runFn2 encodeJsonFn extra) (runFn2 encodeJsonFn extra)

instance (Ord a, EncodeJson extra a) => EncodeJson extra (Set a) where
  encodeJsonFn = mkFn2 \extra -> encodeSet (runFn2 encodeJsonFn extra)

instance (Ord a, EncodeJson extra a) => EncodeJson extra (NonEmptySet a) where
  encodeJsonFn = mkFn2 \extra -> encodeNonEmptySet (runFn2 encodeJsonFn extra)

instance EncodeJson extra CodePoint where
  encodeJsonFn = mkFn2 \_ -> encodeCodePoint

-- | Build a value via `mkExistentialEncoder0`.
foreign import data ExistentialEncoder0 :: Type -> Type

mkExistentialEncoder0 :: forall extra a. (Fn2 extra a Json) -> ExistentialEncoder0 a
mkExistentialEncoder0 = unsafeCoerce

unExistentialEncoder0 :: forall extra a. ExistentialEncoder0 a -> (Fn2 extra a Json)
unExistentialEncoder0 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder0 a) tail rows
  , IsSymbol sym
  ) =>
  EncodeJson extra (K0 sym a) where
  encodeJsonFn = mkFn2 \extra k0a -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      f :: Fn2 extra a Json
      f = unExistentialEncoder0 $ Record.get (Proxy :: Proxy sym) localOverrides

      a :: a
      a = coerce k0a

    runFn2 f extra a

-- | Build a value via `mkExistentialEncoder1`.
foreign import data ExistentialEncoder1 :: (Type -> Type) -> Type

mkExistentialEncoder1 :: forall extra f a. (Fn2 extra a Json) -> (Fn2 extra (f a) Json) -> ExistentialEncoder1 f
mkExistentialEncoder1 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder1 :: forall extra f a. ExistentialEncoder1 f -> ((Fn2 extra a Json) -> (Fn2 extra (f a) Json))
unExistentialEncoder1 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder1 f) tail rows
  , EncodeJson extra a
  , IsSymbol sym
  ) =>
  EncodeJson extra (K1 sym (f a)) where
  encodeJsonFn = mkFn2 \extra k1fa -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildEncoder :: (Fn2 extra a Json) -> (Fn2 extra (f a) Json)
      buildEncoder = unExistentialEncoder1 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJsonFn

      fa :: f a
      fa = coerce k1fa

    runFn2 f extra fa

-- | Build a value via `mkExistentialEncoder2`.
foreign import data ExistentialEncoder2 :: (Type -> Type -> Type) -> Type

mkExistentialEncoder2 :: forall extra f a b. ((Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra (f a b) Json)) -> ExistentialEncoder2 f
mkExistentialEncoder2 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder2 :: forall extra f a b. ExistentialEncoder2 f -> ((Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra (f a b) Json))
unExistentialEncoder2 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialEncoder2 f) tail rows
  , EncodeJson extra a
  , EncodeJson extra b
  , IsSymbol sym
  ) =>
  EncodeJson extra (K2 sym (f a b)) where
  encodeJsonFn = mkFn2 \extra k2fab -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildEncoder :: (Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra (f a b) Json)
      buildEncoder = unExistentialEncoder2 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJsonFn encodeJsonFn

      fab :: f a b
      fab = coerce k2fab

    runFn2 f extra fab

-- | Build a value via `mkExistentialEncoder3`.
foreign import data ExistentialEncoder3 :: (Type -> Type -> Type -> Type) -> Type

mkExistentialEncoder3 :: forall extra f a b c. ((Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra c Json) -> (Fn2 extra (f a b c) Json)) -> ExistentialEncoder3 f
mkExistentialEncoder3 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialEncoder3 :: forall extra f a b c. ExistentialEncoder3 f -> ((Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra c Json) -> (Fn2 extra (f a b c) Json))
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
  encodeJsonFn = mkFn2 \extra k3fabc -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildEncoder :: (Fn2 extra a Json) -> (Fn2 extra b Json) -> (Fn2 extra c Json) -> (Fn2 extra (f a b c) Json)
      buildEncoder = unExistentialEncoder3 $ Record.get (Proxy :: Proxy sym) localOverrides

      f = buildEncoder encodeJsonFn encodeJsonFn encodeJsonFn

      fabc :: f a b c
      fabc = coerce k3fabc

    runFn2 f extra fabc

instance
  ( RowToList.RowToList rows rl
  , EncodeRecordInput rl extra { | rows }
  ) =>
  EncodeJson extra { | rows } where
  encodeJsonFn = mkFn2 \extra input ->
    encodeJObject $ runFn2 encodeRecordInput (RowListRecord input :: RowListRecord rl { | rows }) extra

newtype RowListRecord :: RowList.RowList Type -> Type -> Type
newtype RowListRecord rl rec = RowListRecord rec

class EncodeRecordInput :: RowList.RowList Type -> Type -> Type -> Constraint
class EncodeRecordInput rl extra rec | rl -> extra rec where
  encodeRecordInput :: Fn2 (RowListRecord rl rec) extra (Object Json)

instance EncodeRecordInput RowList.Nil extra {} where
  encodeRecordInput = mkFn2 \_ _ -> Object.empty
else instance
  ( EncodeJson extra a
  , EncodeRecordInput tailList extra { | tailRows }
  , Row.Cons sym (Optional (Maybe a)) tailRows outRows
  , IsSymbol sym
  ) =>
  EncodeRecordInput (RowList.Cons sym (Optional (Maybe a)) tailList) extra { | outRows } where
  encodeRecordInput = mkFn2 \(RowListRecord input) extra -> do
    let
      _sym = Proxy :: Proxy sym
      keyStr = reflectSymbol _sym

      tail :: { | tailRows }
      tail = unsafeCoerce input

      value = unwrap $ Record.get _sym input

      encodeValue = case value of
        Nothing -> identity
        Just a -> Object.insert keyStr (runFn2 encodeJsonFn extra a)

    encodeValue $ runFn2 encodeRecordInput (RowListRecord tail :: RowListRecord tailList { | tailRows }) extra
else instance
  ( EncodeJson extra a
  , EncodeRecordInput tailList extra { | tailRows }
  , Row.Cons sym a tailRows outRows
  , IsSymbol sym
  ) =>
  EncodeRecordInput (RowList.Cons sym a tailList) extra { | outRows } where
  encodeRecordInput = mkFn2 \(RowListRecord input) extra -> do
    let
      _sym = Proxy :: Proxy sym
      keyStr = reflectSymbol _sym

      tail :: { | tailRows }
      tail = unsafeCoerce input

      value :: a
      value = Record.get _sym input
    Object.insert keyStr (runFn2 encodeJsonFn extra value) $ runFn2 encodeRecordInput (RowListRecord tail :: RowListRecord tailList { | tailRows }) extra
