module Json.Decode.Class where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (CodePoint)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Json.Primitive.Decode (JsonDecoder, decodeField', failWithMissingField)
import Json.Types (Optional(..))
import Json.Unidirectional.Decode.Value (class RebuildRecord, decodeArray, decodeBoolean, decodeChar, decodeCodePoint, decodeEither, decodeIdentity, decodeInt, decodeList, decodeMap, decodeMaybeTagged, decodeNonEmpty, decodeNonEmptyArray, decodeNonEmptyList, decodeNonEmptySet, decodeNonEmptyString, decodeNullToUnit, decodeNullable, decodeNumber, decodeObject, decodeRecordPrim, decodeSet, decodeString, decodeThese, decodeTuple, decodeVoid)
import Prim.Coerce (class Coercible)
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Type.Proxy (Proxy(..))

class DecodeJson err a where
  decodeJson :: JsonDecoder err a

instance DecodeJson err Void where
  decodeJson = decodeVoid

instance DecodeJson err Unit where
  decodeJson = decodeNullToUnit

instance DecodeJson err Boolean where
  decodeJson = decodeBoolean

instance DecodeJson err Int where
  decodeJson = decodeInt

instance DecodeJson err Char where
  decodeJson = decodeChar

instance DecodeJson err Number where
  decodeJson = decodeNumber

instance DecodeJson err String where
  decodeJson = decodeString

instance DecodeJson err NonEmptyString where
  decodeJson = decodeNonEmptyString

instance (DecodeJson err a) => DecodeJson err (Array a) where
  decodeJson = decodeArray decodeJson

instance (DecodeJson err a) => DecodeJson err (NonEmptyArray a) where
  decodeJson = decodeNonEmptyArray decodeJson

instance (DecodeJson err a) => DecodeJson err (Object a) where
  decodeJson = decodeObject decodeJson

instance (DecodeJson err a) => DecodeJson err (Nullable a) where
  decodeJson = decodeNullable decodeJson

instance (Coercible (JsonDecoder err a) (JsonDecoder err (Identity a)), DecodeJson err a) => DecodeJson err (Identity a) where
  decodeJson = decodeIdentity decodeJson

instance (DecodeJson err a) => DecodeJson err (Maybe a) where
  decodeJson = decodeMaybeTagged decodeJson

instance (DecodeJson err l, DecodeJson err r) => DecodeJson err (Either l r) where
  decodeJson = decodeEither decodeJson decodeJson

instance (DecodeJson err l, DecodeJson err r) => DecodeJson err (Tuple l r) where
  decodeJson = decodeTuple decodeJson decodeJson

instance (DecodeJson err l, DecodeJson err r) => DecodeJson err (These l r) where
  decodeJson = decodeThese decodeJson decodeJson

instance (DecodeJson err a, DecodeJson err (f a)) => DecodeJson err (NonEmpty f a) where
  decodeJson = decodeNonEmpty decodeJson decodeJson

instance (DecodeJson err a) => DecodeJson err (List a) where
  decodeJson = decodeList decodeJson

instance (DecodeJson err a) => DecodeJson err (NonEmptyList a) where
  decodeJson = decodeNonEmptyList decodeJson

instance (Ord k, DecodeJson err k, DecodeJson err v) => DecodeJson err (Map k v) where
  decodeJson = decodeMap decodeJson decodeJson

instance (Ord a, DecodeJson err a) => DecodeJson err (Set a) where
  decodeJson = decodeSet decodeJson

instance (Ord a, DecodeJson err a) => DecodeJson err (NonEmptySet a) where
  decodeJson = decodeNonEmptySet decodeJson

instance DecodeJson err CodePoint where
  decodeJson = decodeCodePoint

newtype RowListObject :: Type -> RowList.RowList Type -> Type -> Type
newtype RowListObject err rl a = RowListObject (Object a)

instance
  ( RowToList.RowToList rows rl
  , BuildPropDecoders err rl out
  , RebuildRecord out {} { | rows }
  ) =>
  DecodeJson err { | rows } where
  decodeJson = decodeRecordPrim \obj -> buildPropDecoders (RowListObject obj :: RowListObject err rl Json)

class BuildPropDecoders err rl out | err rl -> out where
  buildPropDecoders :: RowListObject err rl Json -> JsonDecoder err out

instance BuildPropDecoders err RowList.Nil Unit where
  buildPropDecoders _ = pure unit
else instance
  ( DecodeJson err a
  , BuildPropDecoders err tailList out
  , IsSymbol sym
  ) =>
  BuildPropDecoders err (RowList.Cons sym (Optional (Maybe a)) tailList) (Tuple (Tuple (Proxy sym) (Optional (Maybe a))) out) where
  buildPropDecoders (RowListObject obj) = ado
    tailRecord <- buildPropDecoders (RowListObject obj :: RowListObject err tailList Json)
    label <- Tuple _sym <$> decodeField' obj keyStr (pure (Optional Nothing)) ((\a -> Optional (Just a)) <$> decodeJson)
    in Tuple label tailRecord
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym
else instance
  ( DecodeJson err a
  , BuildPropDecoders err tailList out
  , IsSymbol sym
  ) =>
  BuildPropDecoders err (RowList.Cons sym a tailList) (Tuple (Tuple (Proxy sym) a) out) where
  buildPropDecoders (RowListObject obj) = ado
    tailRecord <- buildPropDecoders (RowListObject obj :: RowListObject err tailList Json)
    label <- Tuple _sym <$> decodeField' obj keyStr (failWithMissingField keyStr) decodeJson
    in Tuple label tailRecord
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym
