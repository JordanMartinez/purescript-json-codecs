module Codec.Json.Bidirectional.Class
  ( class CodecJson
  , codecJson
  , ExistentialCodecJson0
  , mkExistentialCodecJson0
  , ExistentialCodecJson1
  , mkExistentialCodecJson1
  , ExistentialCodecJson2
  , mkExistentialCodecJson2
  , ExistentialCodecJson3
  , mkExistentialCodecJson3
  , CJPropFn
  , class CodecJsonRecord
  , codecJsonRecord
  , CJVariantFn
  , class CodecJsonVariant
  , codecJsonVariant
  ) where

import Prelude

import Codec.Codec (Codec(..), decoder, encoder, mapDecodeError)
import Codec.Decoder (DecoderFn(..), altAccumulate)
import Codec.Json.Bidirectional.Value (array, boolean, codePoint, either, int, json, list, mapCodec, maybe, nonEmpty', nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, nullable, number, object, recordPrim, requiredProp, set, string, these, tuple, unitCodec, variantCase, variantPrim, voidCodec)
import Codec.Json.JsonCodec (JPropCodec, JsonCodec', JsonCodec, addCtorHintC, addTypeHintC)
import Codec.Json.JsonDecoder (DecodeErrorAccumulatorFn)
import Codec.Json.Newtypes (K0(..), K1(..), K2(..), K3(..), Optional(..))
import Codec.Json.Unidirectional.Decode.Class (class VCTypeHint, VCHint(..), vcTypeHint)
import Codec.Json.Unidirectional.Decode.Value (decodeField')
import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2, mkFn5, runFn2, runFn5)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..))
import Data.List (List)
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (CodePoint)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Data.Variant as V
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Unsafe (unsafeGet, unsafeSet)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
  codecJson = nonEmpty' codecJson codecJson

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

foreign import data ExistentialCodecJson0 :: Type -> Type

mkExistentialCodecJson0 :: forall e extra a. JsonCodec e extra a -> ExistentialCodecJson0 a
mkExistentialCodecJson0 = unsafeCoerce

unExistentialCodecJson0 :: forall e extra a. ExistentialCodecJson0 a -> JsonCodec e extra a
unExistentialCodecJson0 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , Row.Cons sym (ExistentialCodecJson0 a) tail rows
  , IsSymbol sym
  ) =>
  CodecJson e extra (K0 sym a) where
  codecJson = Codec dec enc
    where
    _sym = Proxy :: Proxy sym
    dec = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra
        (DecoderFn f) = decoder $ unExistentialCodecJson0 $ Record.get _sym localOverrides

        reAddNewtype :: V e a -> V e (K0 sym a)
        reAddNewtype = coerce
      reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json
    enc = mkFn2 \extra k0a -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra

        f = encoder $ unExistentialCodecJson0 $ Record.get _sym localOverrides

        a :: a
        a = coerce k0a

      k0a <$ runFn2 f extra a

foreign import data ExistentialCodecJson1 :: (Type -> Type) -> Type

mkExistentialCodecJson1 :: forall e extra f a. (JsonCodec e extra a -> JsonCodec e extra (f a)) -> ExistentialCodecJson1 f
mkExistentialCodecJson1 = unsafeCoerce

unExistentialCodecJson1 :: forall e extra f a. ExistentialCodecJson1 f -> (JsonCodec e extra a -> JsonCodec e extra (f a))
unExistentialCodecJson1 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , CodecJson e extra a
  , Row.Cons sym (ExistentialCodecJson1 f) tail rows
  , IsSymbol sym
  ) =>
  CodecJson e extra (K1 sym (f a)) where
  codecJson = Codec dec enc
    where
    _sym = Proxy :: Proxy sym
    codecJsonA = codecJson :: JsonCodec e extra a
    dec = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra
        buildDecoder = unExistentialCodecJson1 $ Record.get _sym localOverrides
        (DecoderFn f) = decoder $ buildDecoder codecJsonA

        reAddNewtype :: V e (f a) -> V e (K1 sym (f a))
        reAddNewtype = coerce
      reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json
    enc = mkFn2 \extra k1fa -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra

        buildEncoder = unExistentialCodecJson1 $ Record.get _sym localOverrides
        f = encoder $ buildEncoder codecJsonA

        fa :: f a
        fa = coerce k1fa

      k1fa <$ runFn2 f extra fa

foreign import data ExistentialCodecJson2 :: (Type -> Type -> Type) -> Type

mkExistentialCodecJson2 :: forall e extra f a b. (JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra (f a b)) -> ExistentialCodecJson2 f
mkExistentialCodecJson2 = unsafeCoerce

unExistentialCodecJson2 :: forall e extra f a b. ExistentialCodecJson2 f -> (JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra (f a b))
unExistentialCodecJson2 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , CodecJson e extra a
  , CodecJson e extra b
  , Row.Cons sym (ExistentialCodecJson2 f) tail rows
  , IsSymbol sym
  ) =>
  CodecJson e extra (K2 sym (f a b)) where
  codecJson = Codec dec enc
    where
    _sym = Proxy :: Proxy sym
    codecJsonA = codecJson :: JsonCodec e extra a
    codecJsonB = codecJson :: JsonCodec e extra b
    dec = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra
        buildDecoder = unExistentialCodecJson2 $ Record.get _sym localOverrides
        (DecoderFn f) = decoder $ buildDecoder codecJsonA codecJsonB

        reAddNewtype :: V e (f a b) -> V e (K2 sym (f a b))
        reAddNewtype = coerce
      reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json
    enc = mkFn2 \extra k2fab -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra

        buildEncoder = unExistentialCodecJson2 $ Record.get _sym localOverrides
        f = encoder $ buildEncoder codecJsonA codecJsonB

        fab :: f a b
        fab = coerce k2fab

      k2fab <$ runFn2 f extra fab

foreign import data ExistentialCodecJson3 :: (Type -> Type -> Type -> Type) -> Type

mkExistentialCodecJson3 :: forall e extra f a b c. (JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra c -> JsonCodec e extra (f a b c)) -> ExistentialCodecJson3 f
mkExistentialCodecJson3 = unsafeCoerce

unExistentialCodecJson3 :: forall e extra f a b c. ExistentialCodecJson3 f -> (JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra c -> JsonCodec e extra (f a b c))
unExistentialCodecJson3 = unsafeCoerce

instance
  ( Newtype extra { | rows }
  , CodecJson e extra a
  , CodecJson e extra b
  , CodecJson e extra c
  , Row.Cons sym (ExistentialCodecJson3 f) tail rows
  , IsSymbol sym
  ) =>
  CodecJson e extra (K3 sym (f a b c)) where
  codecJson = Codec dec enc
    where
    _sym = Proxy :: Proxy sym
    codecJsonA = codecJson :: JsonCodec e extra a
    codecJsonB = codecJson :: JsonCodec e extra b
    codecJsonC = codecJson :: JsonCodec e extra c
    dec = DecoderFn $ mkFn5 \pathSoFar appendFn handlers extra json -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra
        buildDecoder = unExistentialCodecJson3 $ Record.get _sym localOverrides
        (DecoderFn f) = decoder $ buildDecoder codecJsonA codecJsonB codecJsonC

        reAddNewtype :: V e (f a b c) -> V e (K3 sym (f a b c))
        reAddNewtype = coerce
      reAddNewtype $ runFn5 f pathSoFar appendFn handlers extra json
    enc = mkFn2 \extra k3fabc -> do
      let
        localOverrides :: { | rows }
        localOverrides = unwrap extra

        buildEncoder = unExistentialCodecJson3 $ Record.get _sym localOverrides
        f = encoder $ buildEncoder codecJsonA codecJsonB codecJsonC

        fabc :: f a b c
        fabc = coerce k3fabc

      k3fabc <$ runFn2 f extra fabc

instance CodecJson e extra NoArguments where
  codecJson = dimap (const unit) (const NoArguments) unitCodec

instance (CodecJson e extra a, CodecJson e extra b) => CodecJson e extra (Sum a b) where
  codecJson = addTypeHintC "Sum"
    $ dimap toVariant fromVariant
    $ variantPrim altAccumulate
    $ variantCase _inl (Tuple (addCtorHintC "Inl") $ Right codecJson)
        >>> variantCase _inr (Tuple (addCtorHintC "Inr") $ Right codecJson)
    where
    toVariant = case _ of
      Inl l -> V.inj _inl l
      Inr r -> V.inj _inr r
    fromVariant = V.case_
      # V.on _inl Inl
      # V.on _inr Inr
    _inl = Proxy :: Proxy "Inl"
    _inr = Proxy :: Proxy "Inr"

instance (CodecJson e extra a, CodecJson e extra b) => CodecJson e extra (Product a b) where
  codecJson = addTypeHintC "Product"
    $ dimap (\(Product a b) -> Tuple a b) (\(Tuple a b) -> Product a b)
    $ tuple codecJson codecJson

instance (CodecJson e extra a, IsSymbol sym) => CodecJson e extra (Constructor sym a) where
  codecJson = addTypeHintC ("Constructor (" <> (reflectSymbol (Proxy :: _ sym)) <> ")")
    $ (coerce :: JsonCodec e extra a -> JsonCodec e extra (Constructor sym a)) codecJson

instance (CodecJson e extra a) => CodecJson e extra (Argument a) where
  codecJson = addTypeHintC "Argument"
    $ (coerce :: JsonCodec e extra a -> JsonCodec e extra (Argument a)) codecJson

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

else instance
  ( Row.Cons sym (Optional (Maybe a)) row' row
  , CodecJson e extra a
  , CodecJsonRecord e extra tail row'
  , IsSymbol sym
  ) =>
  CodecJsonRecord e extra (RL.Cons sym (Optional (Maybe a)) tail) row where
  codecJsonRecord = CJPropFn
    ( optionalProp' (Proxy :: Proxy sym) codecJson
        <<< (unCJPropFn (codecJsonRecord :: CJPropFn e extra tail row'))
    )

else instance
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

optionalProp'
  :: forall e extra sym a r r'
   . IsSymbol sym
  => Row.Cons sym (Optional (Maybe a)) r r'
  => Proxy sym
  -> JsonCodec e extra a
  -> JPropCodec e extra { | r }
  -> JPropCodec e extra { | r' }
optionalProp' _sym codecA codecR = Codec dec enc
  where
  key = reflectSymbol _sym
  dec = ado
    r <- decoder codecR
    a <- decodeField' key (pure Nothing) (Just <$> decoder codecA)
    in unsafeSet key (Optional a) r

  enc :: Fn2 extra { | r' } (Tuple (List (Tuple String Json)) { | r' })
  enc = mkFn2 \extra val -> do
    let tail = fst $ runFn2 (encoder codecR) extra (unsafeForget val)
    let mbHead = map (\a -> Tuple key (fst $ runFn2 (encoder codecA) extra a)) $ unsafeGet key val
    Tuple (Maybe.maybe tail (\h -> h List.: tail) mbHead) val

  unsafeForget :: Record r' → Record r
  unsafeForget = unsafeCoerce

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
  , VCTypeHint e extra (RL.Cons sym a tail) row a
  ) =>
  CodecJsonVariant e extra (RL.Cons sym a tail) row where
  codecJsonVariant = CJVariantFn
    ( variantCase (Proxy :: Proxy sym) (Tuple (mapDecodeError addHint) $ Right codecJson)
        <<< (unCJVariantFn (codecJsonVariant :: CJVariantFn e extra tail row'))
    )
    where
    addHint = vcTypeHint (VCHint :: VCHint e extra (RL.Cons sym a tail) row a)
