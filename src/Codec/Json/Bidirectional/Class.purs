module Codec.Json.Bidirectional.Class where

import Prelude

import Codec.Decoder (altAccumulate)
import Codec.Json.Bidirectional.Value (array, boolean, codePoint, either, int, json, list, mapCodec, maybe, nonEmpty, nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, nullable, number, object, recordPrim, requiredProp, set, string, these, tuple, unitCodec, variantCase, variantPrim, voidCodec)
import Codec.Json.JsonCodec (JPropCodec, JsonCodec, JsonCodec')
import Codec.Json.JsonDecoder (DecodeErrorAccumulatorFn)
import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
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
import Data.Variant (Variant)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class CodecJson e extra a where
  codecJson :: JsonCodec e extra a

instance CodecJson e extra Json where
  codecJson = json

instance CodecJson e extra Void where
  codecJson = voidCodec

instance CodecJson e extra Unit where
  codecJson = unitCodec

instance CodecJson e extra Boolean where
  codecJson = boolean

instance CodecJson e extra Number where
  codecJson = number

instance CodecJson e extra String where
  codecJson = string

instance CodecJson e extra Int where
  codecJson = int

instance CodecJson e extra NonEmptyString where
  codecJson = nonEmptyString

instance CodecJson e extra CodePoint where
  codecJson = codePoint

instance CodecJson e extra a => CodecJson e extra (Array a) where
  codecJson = array codecJson

instance CodecJson e extra a => CodecJson e extra (NonEmptyArray a) where
  codecJson = nonEmptyArray codecJson

instance CodecJson e extra a => CodecJson e extra (Object a) where
  codecJson = object codecJson

instance CodecJson e extra a => CodecJson e extra (Nullable a) where
  codecJson = nullable codecJson

instance CodecJson e extra a => CodecJson e extra (Maybe a) where
  codecJson = maybe codecJson

instance (CodecJson e extra a, CodecJson e extra b) => CodecJson e extra (Either a b) where
  codecJson = either codecJson codecJson

instance (CodecJson e extra a, CodecJson e extra b) => CodecJson e extra (Tuple a b) where
  codecJson = tuple codecJson codecJson

instance (CodecJson e extra a, CodecJson e extra b) => CodecJson e extra (These a b) where
  codecJson = these codecJson codecJson

instance (CodecJson e extra a, CodecJson e extra (f a)) => CodecJson e extra (NonEmpty f a) where
  codecJson = nonEmpty codecJson codecJson

instance (CodecJson e extra a) => CodecJson e extra (List a) where
  codecJson = list codecJson

instance (CodecJson e extra a) => CodecJson e extra (NonEmptyList a) where
  codecJson = nonEmptyList codecJson

instance (Ord k, CodecJson e extra k, CodecJson e extra v) => CodecJson e extra (Map k v) where
  codecJson = mapCodec codecJson codecJson

instance (Ord a, CodecJson e extra a) => CodecJson e extra (Set a) where
  codecJson = set codecJson

instance (Ord a, CodecJson e extra a) => CodecJson e extra (NonEmptySet a) where
  codecJson = nonEmptySet codecJson

instance
  ( RL.RowToList row rl
  , CodecJsonRecord e extra rl row
  ) =>
  CodecJson e extra { | row } where
  codecJson = recordPrim (unCJPropFn (codecJsonRecord :: CJPropFn e extra rl row))

instance
  ( RL.RowToList row rl
  , CodecJsonVariant e extra rl row
  ) =>
  CodecJson e extra (Variant row) where
  codecJson = variantPrim altAccumulate (unCJVariantFn (codecJsonVariant :: CJVariantFn e extra rl row))

newtype CJPropFn :: Type -> Type -> RL.RowList Type -> Row Type -> Type
newtype CJPropFn e extra rl to =
  CJPropFn (JPropCodec e extra {} -> JPropCodec e extra { | to })

unCJPropFn
  :: forall e extra rl to
   . CJPropFn e extra rl to
  -> (JPropCodec e extra {} -> JPropCodec e extra { | to })
unCJPropFn (CJPropFn fn) = fn

class CodecJsonRecord e extra rl row | e extra rl -> row where
  codecJsonRecord :: CJPropFn e extra rl row

instance CodecJsonRecord e extra RL.Nil () where
  codecJsonRecord = CJPropFn identity

instance
  ( Row.Cons sym a row' row
  , CodecJson e extra a
  , CodecJsonRecord e extra tail row'
  , IsSymbol sym
  ) =>
  CodecJsonRecord e extra (RL.Cons sym a tail) row where
  codecJsonRecord = CJPropFn
    ( requiredProp (Proxy :: Proxy sym) codecJson
        <<< (unCJPropFn (codecJsonRecord :: CJPropFn e extra tail row'))
    )

newtype CJVariantFn :: Type -> Type -> RL.RowList Type -> Row Type -> Type
newtype CJVariantFn e extra rl rows = CJVariantFn
  ( ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant ())
      -> JsonCodec' e extra (Object Json) (Variant ())
    )
    -> ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows)
         -> JsonCodec' e extra (Object Json) (Variant rows)
       )
  )

unCJVariantFn
  :: forall e extra rl rows
   . CJVariantFn e extra rl rows
  -> ( ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant ())
         -> JsonCodec' e extra (Object Json) (Variant ())
       )
       -> ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows)
            -> JsonCodec' e extra (Object Json) (Variant rows)
          )
     )
unCJVariantFn (CJVariantFn fn) = fn

class CodecJsonVariant e extra rl row | e extra rl -> row where
  codecJsonVariant :: CJVariantFn e extra rl row

instance CodecJsonVariant e extra RL.Nil () where
  codecJsonVariant = CJVariantFn \buildTailCodec errorAccumulator ->
    buildTailCodec errorAccumulator

instance
  ( Row.Cons sym a row' row
  , CodecJson e extra a
  , CodecJsonVariant e extra tail row'
  , IsSymbol sym
  ) =>
  CodecJsonVariant e extra (RL.Cons sym a tail) row where
  codecJsonVariant = CJVariantFn
    ( variantCase (Proxy :: Proxy sym) (Right codecJson)
        <<< (unCJVariantFn (codecJsonVariant :: CJVariantFn e extra tail row'))
    )

-- Generic
