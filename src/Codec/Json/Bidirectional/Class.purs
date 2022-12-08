module Codec.Json.Bidirectional.Class where

import Prelude

import Codec.Json.Bidirectional.Value (array, boolean, codePoint, either, int, json, list, mapCodec, maybe, nonEmpty, nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, nullable, number, object, set, string, these, tuple, unitCodec, voidCodec)
import Codec.Json.JsonCodec (JsonCodec)
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
import Data.These (These)
import Data.Tuple (Tuple)
import Foreign.Object (Object)

class CodecJson a where
  codecJson :: forall e extra. JsonCodec e extra a

instance CodecJson Json where
  codecJson = json

instance CodecJson Void where
  codecJson = voidCodec

instance CodecJson Unit where
  codecJson = unitCodec

instance CodecJson Boolean where
  codecJson = boolean

instance CodecJson Number where
  codecJson = number

instance CodecJson String where
  codecJson = string

instance CodecJson Int where
  codecJson = int

instance CodecJson NonEmptyString where
  codecJson = nonEmptyString

instance CodecJson CodePoint where
  codecJson = codePoint

instance CodecJson a => CodecJson (Array a) where
  codecJson = array codecJson

instance CodecJson a => CodecJson (NonEmptyArray a) where
  codecJson = nonEmptyArray codecJson

-- Record

instance CodecJson a => CodecJson (Object a) where
  codecJson = object codecJson

-- Variant

instance CodecJson a => CodecJson (Nullable a) where
  codecJson = nullable codecJson

instance CodecJson a => CodecJson (Maybe a) where
  codecJson = maybe codecJson

instance (CodecJson a, CodecJson b) => CodecJson (Either a b) where
  codecJson = either codecJson codecJson

instance (CodecJson a, CodecJson b) => CodecJson (Tuple a b) where
  codecJson = tuple codecJson codecJson

instance (CodecJson a, CodecJson b) => CodecJson (These a b) where
  codecJson = these codecJson codecJson

instance (CodecJson a, CodecJson (f a)) => CodecJson (NonEmpty f a) where
  codecJson = nonEmpty codecJson codecJson

instance (CodecJson a) => CodecJson (List a) where
  codecJson = list codecJson

instance (CodecJson a) => CodecJson (NonEmptyList a) where
  codecJson = nonEmptyList codecJson

instance (Ord k, CodecJson k, CodecJson v) => CodecJson (Map k v) where
  codecJson = mapCodec codecJson codecJson

instance (Ord a, CodecJson a) => CodecJson (Set a) where
  codecJson = set codecJson

instance (Ord a, CodecJson a) => CodecJson (NonEmptySet a) where
  codecJson = nonEmptySet codecJson

-- Generic
