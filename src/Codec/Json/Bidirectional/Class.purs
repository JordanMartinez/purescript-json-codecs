module Codec.Json.Bidirectional.Class where

import Prelude

import Codec.Json.Bidirectional.Value (array, boolean, codePoint, either, int, json, list, mapCodec, maybe, nonEmpty, nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, nullable, number, object, recordPrim, requiredProp, set, string, these, tuple, unitCodec, voidCodec)
import Codec.Json.JsonCodec (JPropCodec, JsonCodec)
import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
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
import Data.Symbol (class IsSymbol)
import Data.These (These)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class CodecJon e extra a where
  codecJson :: JsonCodec e extra a

instance CodecJon e extra Json where
  codecJson = json

instance CodecJon e extra Void where
  codecJson = voidCodec

instance CodecJon e extra Unit where
  codecJson = unitCodec

instance CodecJon e extra Boolean where
  codecJson = boolean

instance CodecJon e extra Number where
  codecJson = number

instance CodecJon e extra String where
  codecJson = string

instance CodecJon e extra Int where
  codecJson = int

instance CodecJon e extra NonEmptyString where
  codecJson = nonEmptyString

instance CodecJon e extra CodePoint where
  codecJson = codePoint

instance CodecJon e extra a => CodecJon e extra (Array a) where
  codecJson = array codecJson

instance CodecJon e extra a => CodecJon e extra (NonEmptyArray a) where
  codecJson = nonEmptyArray codecJson

instance CodecJon e extra a => CodecJon e extra (Object a) where
  codecJson = object codecJson

instance CodecJon e extra a => CodecJon e extra (Nullable a) where
  codecJson = nullable codecJson

instance CodecJon e extra a => CodecJon e extra (Maybe a) where
  codecJson = maybe codecJson

instance (CodecJon e extra a, CodecJon e extra b) => CodecJon e extra (Either a b) where
  codecJson = either codecJson codecJson

instance (CodecJon e extra a, CodecJon e extra b) => CodecJon e extra (Tuple a b) where
  codecJson = tuple codecJson codecJson

instance (CodecJon e extra a, CodecJon e extra b) => CodecJon e extra (These a b) where
  codecJson = these codecJson codecJson

instance (CodecJon e extra a, CodecJon e extra (f a)) => CodecJon e extra (NonEmpty f a) where
  codecJson = nonEmpty codecJson codecJson

instance (CodecJon e extra a) => CodecJon e extra (List a) where
  codecJson = list codecJson

instance (CodecJon e extra a) => CodecJon e extra (NonEmptyList a) where
  codecJson = nonEmptyList codecJson

instance (Ord k, CodecJon e extra k, CodecJon e extra v) => CodecJon e extra (Map k v) where
  codecJson = mapCodec codecJson codecJson

instance (Ord a, CodecJon e extra a) => CodecJon e extra (Set a) where
  codecJson = set codecJson

instance (Ord a, CodecJon e extra a) => CodecJon e extra (NonEmptySet a) where
  codecJson = nonEmptySet codecJson

instance
  ( RL.RowToList row rl
  , CodecJsonRecord e extra rl row
  ) =>
  CodecJon e extra { | row } where
  codecJson = recordPrim (unCJPropFn (codecJsonRecord :: CJPropFn e extra rl () row))

newtype CJPropFn :: Type -> Type -> RL.RowList Type -> Row Type -> Row Type -> Type
newtype CJPropFn e extra rl from to =
  CJPropFn (JPropCodec e extra { | from } -> JPropCodec e extra { | to })

unCJPropFn
  :: forall e extra rl from to
   . CJPropFn e extra rl from to
  -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })
unCJPropFn (CJPropFn fn) = fn

class CodecJsonRecord e extra rl row | e extra rl -> row where
  codecJsonRecord :: CJPropFn e extra rl () row

instance CodecJsonRecord e extra RL.Nil () where
  codecJsonRecord = CJPropFn identity

-- HUH!? Compiler: "Unknown type class 'CodecJson'"
--
-- instance
--   ( Row.Cons sym a row' row
--   , CodecJson e extra a
--   , CodecJsonRecord e extra tail row'
--   , IsSymbol sym
--   ) =>
--   CodecJsonRecord e extra (RL.Cons sym a tail) row where
--   codecJsonRecord = CJPropFn
--     ( requiredProp (Proxy :: Proxy sym) codecJson
--         <<< (unCJPropFn (codecJsonRecord :: CJPropFn e extra tail () row'))
--     )

-- Variant

-- Generic
