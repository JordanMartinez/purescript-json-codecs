module Json.Decode.Class where

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
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Json.Primitive.Decode (class IsDecodeJsonError, JsonDecoder, decodeField', failWithPath, onMissingField)
import Json.Unidirectional.Decode.Value (class RebuildRecord, decodeArray, decodeBoolean, decodeChar, decodeCodePoint, decodeEither, decodeIdentity, decodeInt, decodeList, decodeMap, decodeMaybeTagged, decodeNonEmpty, decodeNonEmptyArray, decodeNonEmptyList, decodeNonEmptySet, decodeNonEmptyString, decodeNullToUnit, decodeNullable, decodeNumber, decodeObject, decodeRecordPrim, decodeSet, decodeString, decodeThese, decodeTuple, decodeVoid)
import Prim.Coerce (class Coercible)
import Prim.RowList as RowList
import Prim.RowList as RowToList
import Type.Proxy (Proxy(..))

class DecodeJson err a where
  decodeJson :: JsonDecoder err a

instance IsDecodeJsonError err => DecodeJson err Void where
  decodeJson = decodeVoid

instance IsDecodeJsonError err => DecodeJson err Unit where
  decodeJson = decodeNullToUnit

instance IsDecodeJsonError err => DecodeJson err Boolean where
  decodeJson = decodeBoolean

instance IsDecodeJsonError err => DecodeJson err Int where
  decodeJson = decodeInt

instance IsDecodeJsonError err => DecodeJson err Char where
  decodeJson = decodeChar

instance IsDecodeJsonError err => DecodeJson err Number where
  decodeJson = decodeNumber

instance IsDecodeJsonError err => DecodeJson err String where
  decodeJson = decodeString

instance IsDecodeJsonError err => DecodeJson err NonEmptyString where
  decodeJson = decodeNonEmptyString

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Array a) where
  decodeJson = decodeArray decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (NonEmptyArray a) where
  decodeJson = decodeNonEmptyArray decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Object a) where
  decodeJson = decodeObject decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Nullable a) where
  decodeJson = decodeNullable decodeJson

instance (Coercible (JsonDecoder err a) (JsonDecoder err (Identity a)), DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Identity a) where
  decodeJson = decodeIdentity decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Maybe a) where
  decodeJson = decodeMaybeTagged decodeJson

instance (DecodeJson err l, DecodeJson err r, IsDecodeJsonError err) => DecodeJson err (Either l r) where
  decodeJson = decodeEither decodeJson decodeJson

instance (DecodeJson err l, DecodeJson err r, IsDecodeJsonError err) => DecodeJson err (Tuple l r) where
  decodeJson = decodeTuple decodeJson decodeJson

instance (DecodeJson err l, DecodeJson err r, IsDecodeJsonError err) => DecodeJson err (These l r) where
  decodeJson = decodeThese decodeJson decodeJson

instance (DecodeJson err a, DecodeJson err (f a), IsDecodeJsonError err) => DecodeJson err (NonEmpty f a) where
  decodeJson = decodeNonEmpty decodeJson decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (List a) where
  decodeJson = decodeList decodeJson

instance (DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (NonEmptyList a) where
  decodeJson = decodeNonEmptyList decodeJson

instance (Ord k, DecodeJson err k, DecodeJson err v, IsDecodeJsonError err) => DecodeJson err (Map k v) where
  decodeJson = decodeMap decodeJson decodeJson

instance (Ord a, DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (Set a) where
  decodeJson = decodeSet decodeJson

instance (Ord a, DecodeJson err a, IsDecodeJsonError err) => DecodeJson err (NonEmptySet a) where
  decodeJson = decodeNonEmptySet decodeJson

instance IsDecodeJsonError err => DecodeJson err CodePoint where
  decodeJson = decodeCodePoint

newtype RowListObject :: Type -> RowList.RowList Type -> Type -> Type
newtype RowListObject err rl a = RowListObject (Object a)

instance
  ( RowToList.RowToList rows rl
  , IsDecodeJsonError err
  , BuildPropDecoders err rl out
  , RebuildRecord out {} { | rows }
  ) =>
  DecodeJson err { | rows } where
  decodeJson = decodeRecordPrim \obj -> buildPropDecoders (RowListObject obj :: RowListObject err rl Json)

class BuildPropDecoders err rl out | err rl -> out where
  buildPropDecoders :: RowListObject err rl Json -> JsonDecoder err out

-- I think I need to revert back to using `Validation`
-- and then define a special function that allows
-- a `These`-like accumulation for `Alt` specifically.
instance IsDecodeJsonError err => BuildPropDecoders err RowList.Nil Unit where
  buildPropDecoders _ = pure unit
else instance
  ( DecodeJson err a
  , BuildPropDecoders err tailList out
  , IsSymbol sym
  , IsDecodeJsonError err
  ) =>
  BuildPropDecoders err (RowList.Cons sym a tailList) (Tuple (Tuple (Proxy sym) a) out) where
  buildPropDecoders (RowListObject obj) = ado
    tailRecord <- buildPropDecoders (RowListObject obj :: RowListObject err tailList Json)
    label <- Tuple _sym <$> decodeField' obj keyStr (failWithPath (flip onMissingField keyStr)) decodeJson
    in Tuple label tailRecord
    where
    _sym = Proxy :: Proxy sym
    keyStr = reflectSymbol _sym
