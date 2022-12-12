module Codec.Json.Unidirectional.Decode.Class
  ( class DecodeJson
  , decodeJson
  , ExistentialDecoder0
  , mkExistentialDecoder0
  , ExistentialDecoder1
  , mkExistentialDecoder1
  , ExistentialDecoder2
  , mkExistentialDecoder2
  , ExistentialDecoder3
  , mkExistentialDecoder3
  , RowListJsonObjDecoder
  , class BuildPropDecoders
  , buildPropDecoders
  ) where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Codec.Json.JsonDecoder (DecodeErrorAccumulatorFn, JsonDecoder, JsonDecoder', altAccumulate, failWithMissingField)
import Codec.Json.Newtypes (K0(..), K1(..), K2(..), K3(..), Optional(..))
import Codec.Json.Unidirectional.Decode.Value (decodeArray, decodeBoolean, decodeChar, decodeCodePoint, decodeEither, decodeField', decodeIdentity, decodeInt, decodeList, decodeMap, decodeMaybeTagged, decodeNonEmpty, decodeNonEmptyArray, decodeNonEmptyList, decodeNonEmptySet, decodeNonEmptyString, decodeNullable, decodeNumber, decodeObject, decodeRecordPrim, decodeSet, decodeString, decodeThese, decodeTuple, decodeUnitFromNull, decodeVariantPrim, decodeVariantCase, decodeVoid)
import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn5, runFn5)
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
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Record as Record
import Record.Builder (Builder, buildFromScratch)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class DecodeJson err extra a where
  decodeJson :: JsonDecoder err extra a

instance DecodeJson err extra Json where
  decodeJson = identity

instance DecodeJson err extra Void where
  decodeJson = decodeVoid

instance DecodeJson err extra Unit where
  decodeJson = decodeUnitFromNull

instance DecodeJson err extra Boolean where
  decodeJson = decodeBoolean

instance DecodeJson err extra Int where
  decodeJson = decodeInt

instance DecodeJson err extra Char where
  decodeJson = decodeChar

instance DecodeJson err extra Number where
  decodeJson = decodeNumber

instance DecodeJson err extra String where
  decodeJson = decodeString

instance DecodeJson err extra NonEmptyString where
  decodeJson = decodeNonEmptyString

instance (DecodeJson err extra a) => DecodeJson err extra (Array a) where
  decodeJson = decodeArray decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (NonEmptyArray a) where
  decodeJson = decodeNonEmptyArray decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (Object a) where
  decodeJson = decodeObject decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (Nullable a) where
  decodeJson = decodeNullable decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (Identity a) where
  decodeJson = decodeIdentity decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (Maybe a) where
  decodeJson = decodeMaybeTagged decodeJson

instance (DecodeJson err extra l, DecodeJson err extra r) => DecodeJson err extra (Either l r) where
  decodeJson = decodeEither decodeJson decodeJson

instance (DecodeJson err extra l, DecodeJson err extra r) => DecodeJson err extra (Tuple l r) where
  decodeJson = decodeTuple decodeJson decodeJson

instance (DecodeJson err extra l, DecodeJson err extra r) => DecodeJson err extra (These l r) where
  decodeJson = decodeThese decodeJson decodeJson

instance (DecodeJson err extra a, DecodeJson err extra (f a)) => DecodeJson err extra (NonEmpty f a) where
  decodeJson = decodeNonEmpty decodeJson decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (List a) where
  decodeJson = decodeList decodeJson

instance (DecodeJson err extra a) => DecodeJson err extra (NonEmptyList a) where
  decodeJson = decodeNonEmptyList decodeJson

instance (Ord k, DecodeJson err extra k, DecodeJson err extra v) => DecodeJson err extra (Map k v) where
  decodeJson = decodeMap decodeJson decodeJson

instance (Ord a, DecodeJson err extra a) => DecodeJson err extra (Set a) where
  decodeJson = decodeSet decodeJson

instance (Ord a, DecodeJson err extra a) => DecodeJson err extra (NonEmptySet a) where
  decodeJson = decodeNonEmptySet decodeJson

instance DecodeJson err extra CodePoint where
  decodeJson = decodeCodePoint

-- | Build a value via `mkExistentialDecoder0`.
foreign import data ExistentialDecoder0 :: Type -> Type

mkExistentialDecoder0 :: forall err extra a. JsonDecoder err extra a -> ExistentialDecoder0 a
mkExistentialDecoder0 = unsafeCoerce

unExistentialDecoder0 :: forall err extra a. ExistentialDecoder0 a -> JsonDecoder err extra a
unExistentialDecoder0 = unsafeCoerce

-- | Local overrides for types with kind `Type`. Implementation for types annotated with the `K0` type 
-- | are provided via the `extra` label in `JsonDecoderInput`. Value for `extra` must be a
-- | newtyped record. The `sym` in `K0 sym a` must be the label name witin that newtyped record
-- | that contains the local override (i.e. `JsonDecoder err extra a`).
instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialDecoder0 a) tail rows
  , IsSymbol sym
  ) =>
  DecodeJson err extra (K0 sym a) where
  decodeJson = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra
      (DecoderFn f) = unExistentialDecoder0 $ Record.get (Proxy :: Proxy sym) localOverrides

      reAddNewtype :: V err a -> V err (K0 sym a)
      reAddNewtype = coerce
    reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json

-- | Build a value via `mkExistentialDecoder1`.
foreign import data ExistentialDecoder1 :: (Type -> Type) -> Type

mkExistentialDecoder1 :: forall err extra f a. (JsonDecoder err extra a -> JsonDecoder err extra (f a)) -> ExistentialDecoder1 f
mkExistentialDecoder1 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialDecoder1 :: forall err extra f a. ExistentialDecoder1 f -> (JsonDecoder err extra a -> JsonDecoder err extra (f a))
unExistentialDecoder1 = unsafeCoerce

-- | Local overrides for types with kind `Type`. Implementation for types annotated with the `K1` type 
-- | are provided via the `extra` label in `JsonDecoderInput`. Value for `extra` must be a
-- | newtyped record. The `sym` in `K1 sym a` must be the label name witin that newtyped record
-- | that contains the function, `JsonDecoder err extra a -> JsonDecoder err extra (f a)`. The input
-- | argument is the corresponding `DecodeJson` implementation for `a`. Thus, one is only overriding
-- | the implementation for `f`, not `a` as well.
instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialDecoder1 f) tail rows
  , DecodeJson err extra a
  , IsSymbol sym
  ) =>
  DecodeJson err extra (K1 sym (f a)) where
  decodeJson = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildDecoder :: JsonDecoder err extra a -> JsonDecoder err extra (f a)
      buildDecoder = unExistentialDecoder1 $ Record.get (Proxy :: Proxy sym) localOverrides

      (DecoderFn f) = buildDecoder decodeJson

      reAddNewtype :: V err (f a) -> V err (K1 sym (f a))
      reAddNewtype = coerce
    reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json

-- | Build a value via `mkExistentialDecoder2`.
foreign import data ExistentialDecoder2 :: (Type -> Type -> Type) -> Type

mkExistentialDecoder2 :: forall err extra f a b. (JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra (f a b)) -> ExistentialDecoder2 f
mkExistentialDecoder2 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialDecoder2 :: forall err extra f a b. ExistentialDecoder2 f -> (JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra (f a b))
unExistentialDecoder2 = unsafeCoerce

-- | Local overrides for types with kind `Type`. Implementation for types annotated with the `K2` type 
-- | are provided via the `extra` label in `JsonDecoderInput`. Value for `extra` must be a
-- | newtyped record. The `sym` in `K2 sym a` must be the label name witin that newtyped record
-- | that contains the function, `JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra (f a b)`. The input
-- | arguments are the corresponding `DecodeJson` implementations for `a` and `b`. Thus, one is only overriding
-- | the implementation for `f`, not `a` and `b` as well.
instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialDecoder2 f) tail rows
  , DecodeJson err extra a
  , DecodeJson err extra b
  , IsSymbol sym
  ) =>
  DecodeJson err extra (K2 sym (f a b)) where
  decodeJson = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildDecoder :: JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra (f a b)
      buildDecoder = unExistentialDecoder2 $ Record.get (Proxy :: Proxy sym) localOverrides

      (DecoderFn f) = buildDecoder decodeJson decodeJson

      reAddNewtype :: V err (f a b) -> V err (K2 sym (f a b))
      reAddNewtype = coerce
    reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json

-- | Build a value via `mkExistentialDecoder3`.
foreign import data ExistentialDecoder3 :: (Type -> Type -> Type -> Type) -> Type

mkExistentialDecoder3 :: forall err extra f a b c. (JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra c -> JsonDecoder err extra (f a b c)) -> ExistentialDecoder3 f
mkExistentialDecoder3 = unsafeCoerce

-- Note: this is intentionaly not exported as only this module should consume this.
unExistentialDecoder3 :: forall err extra f a b c. ExistentialDecoder3 f -> (JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra c -> JsonDecoder err extra (f a b c))
unExistentialDecoder3 = unsafeCoerce

-- | Local overrides for types with kind `Type`. Implementation for types annotated with the `K3` type 
-- | are provided via the `extra` label in `JsonDecoderInput`. Value for `extra` must be a
-- | newtyped record. The `sym` in `K3 sym a` must be the label name witin that newtyped record
-- | that contains the function, `JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra c -> JsonDecoder err extra (f a b c)`. The input
-- | arguments are the corresponding `DecodeJson` implementations for `a`, `b`, and `c`. Thus, one is only overriding
-- | the implementation for `f`, not `a`, `b`, and `c` as well.
instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialDecoder3 f) tail rows
  , DecodeJson err extra a
  , DecodeJson err extra b
  , DecodeJson err extra c
  , IsSymbol sym
  ) =>
  DecodeJson err extra (K3 sym (f a b c)) where
  decodeJson = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
    let
      localOverrides :: { | rows }
      localOverrides = unwrap extra

      buildDecoder :: JsonDecoder err extra a -> JsonDecoder err extra b -> JsonDecoder err extra c -> JsonDecoder err extra (f a b c)
      buildDecoder = unExistentialDecoder3 $ Record.get (Proxy :: Proxy sym) localOverrides

      (DecoderFn f) = buildDecoder decodeJson decodeJson decodeJson

      reAddNewtype :: V err (f a b c) -> V err (K3 sym (f a b c))
      reAddNewtype = coerce
    reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json

instance
  ( RowToList.RowToList rows rl
  , BuildPropDecoders err extra rl rows
  ) =>
  DecodeJson err extra { | rows } where
  decodeJson =
    decodeRecordPrim
      $ map buildFromScratch
      $ unwrapRLJOD (buildPropDecoders :: RowListJsonObjDecoder err extra rl rows)

instance
  ( RowToList.RowToList rows rl
  , BuildVariantDecoder err extra rl rows
  ) =>
  DecodeJson err extra (Variant rows) where
  decodeJson = decodeVariantPrim altAccumulate (unBvdFn (buildVariantDecoder :: BVDFn err extra rl rows))

newtype RowListJsonObjDecoder :: Type -> Type -> RowList.RowList Type -> Row Type -> Type
newtype RowListJsonObjDecoder err extra rl rows = RowListJsonObjDecoder (JsonDecoder' err extra (Object Json) (Builder {} { | rows }))

unwrapRLJOD
  :: forall err extra rl rows
   . RowListJsonObjDecoder err extra rl rows
  -> JsonDecoder' err extra (Object Json) (Builder {} { | rows })
unwrapRLJOD (RowListJsonObjDecoder codec) = codec

class BuildPropDecoders :: Type -> Type -> RowList.RowList Type -> Row Type -> Constraint
class BuildPropDecoders err extra rl out | err extra rl -> out where
  buildPropDecoders :: RowListJsonObjDecoder err extra rl out

instance BuildPropDecoders err extra RowList.Nil () where
  buildPropDecoders = RowListJsonObjDecoder (pure identity)
else instance
  ( DecodeJson err extra a
  , BuildPropDecoders err extra tailList intermediate
  , Row.Lacks sym intermediate
  , Row.Cons sym (Optional (Maybe a)) intermediate out
  , IsSymbol sym
  ) =>
  BuildPropDecoders err extra (RowList.Cons sym (Optional (Maybe a)) tailList) out where
  buildPropDecoders = RowListJsonObjDecoder ado
    tailBuilder <- unwrapRLJOD (buildPropDecoders :: RowListJsonObjDecoder err extra tailList intermediate)
    value <- decodeField' keyStr (pure (Optional Nothing)) ((\a -> Optional (Just a)) <$> decodeJson)
    in Builder.insert _sym value <<< tailBuilder
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym
else instance
  ( DecodeJson err extra a
  , BuildPropDecoders err extra tailList intermediate
  , Row.Lacks sym intermediate
  , Row.Cons sym a intermediate out
  , IsSymbol sym
  ) =>
  BuildPropDecoders err extra (RowList.Cons sym a tailList) out where
  buildPropDecoders = RowListJsonObjDecoder ado
    tailBuilder <- unwrapRLJOD (buildPropDecoders :: RowListJsonObjDecoder err extra tailList intermediate)
    value <- decodeField' keyStr (failWithMissingField keyStr) decodeJson
    in Builder.insert _sym value <<< tailBuilder
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym

newtype BVDFn :: Type -> Type -> RL.RowList Type -> Row Type -> Type
newtype BVDFn e extra rowlist out = BVDFn
  ( (DecodeErrorAccumulatorFn e extra (Object Json) (Variant ()) -> JsonDecoder' e extra (Object Json) (Variant ()))
    -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant out) -> JsonDecoder' e extra (Object Json) (Variant out))
  )

unBvdFn
  :: forall e extra rowlist out
   . BVDFn e extra rowlist out
  -> ( (DecodeErrorAccumulatorFn e extra (Object Json) (Variant ()) -> JsonDecoder' e extra (Object Json) (Variant ()))
       -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant out) -> JsonDecoder' e extra (Object Json) (Variant out))
     )
unBvdFn (BVDFn fn) = fn

class BuildVariantDecoder :: Type -> Type -> RL.RowList Type -> Row Type -> Constraint
class BuildVariantDecoder e extra rowlist out | e extra rowlist -> out where
  buildVariantDecoder :: BVDFn e extra rowlist out

instance buildVariantDecoderNil :: BuildVariantDecoder e extra RL.Nil () where
  buildVariantDecoder = BVDFn $ \buildTailCodec errorAccumulator -> buildTailCodec errorAccumulator

instance buildVariantDecoderCons ::
  ( DecodeJson e extra a
  , BuildVariantDecoder e extra tail out'
  , Row.Cons sym a out' out
  , IsSymbol sym
  ) =>
  BuildVariantDecoder e extra (RL.Cons sym a tail) out where
  buildVariantDecoder = BVDFn $
    (unBvdFn (buildVariantDecoder :: BVDFn e extra tail out'))
      >>> decodeVariantCase _sym (Right decodeJson)
    where
    _sym = Proxy :: Proxy sym
