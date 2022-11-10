module Json.Bidirectional.Value where

import Prelude

import Codec.Codec (Codec(..), codec, codec', decoder, encoder, (>~>))
import Codec.Decoder (altAccumulate)
import Codec.Decoder.Qualified as Decoder
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (note)
import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor (dimap)
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Json.JsonCodec (JIndexedCodec, JsonCodec, JPropCodec, mkJsonCodec, refinedValue)
import Json.JsonDecoder (addOffset)
import Json.Types (JsonOffset(..))
import Json.Unidirectional.Decode.Value (decodeArrayPrim, decodeBoolean, decodeField, decodeIndex, decodeNull, decodeNumber, decodeObjectPrim, decodeString, decodeVoid)
import Json.Unidirectional.Encode.Value (encodeArrayPrim, encodeBoolean, encodeNumber, encodeObjectPrim, encodeString, encodeUnitToNull, encodeVoid)
import Safe.Coerce (coerce)

jsonCodec :: forall e extra. JsonCodec e extra Json
jsonCodec = mkJsonCodec identity identity

voidCodec :: forall e extra. JsonCodec e extra Void
voidCodec = mkJsonCodec decodeVoid encodeVoid

nullCodec :: forall e extra. JsonCodec e extra Unit
nullCodec = mkJsonCodec decodeNull encodeUnitToNull

booleanCodec :: forall e extra. JsonCodec e extra Boolean
booleanCodec = mkJsonCodec decodeBoolean encodeBoolean

numberCodec :: forall e extra. JsonCodec e extra Number
numberCodec = mkJsonCodec decodeNumber encodeNumber

stringCodec :: forall e extra. JsonCodec e extra String
stringCodec = mkJsonCodec decodeString encodeString

arrayPrimCodec :: forall e extra. JsonCodec e extra (Array Json)
arrayPrimCodec = mkJsonCodec decodeArrayPrim encodeArrayPrim

indexedArrayCodec :: forall e extra a. JIndexedCodec e extra a -> JsonCodec e extra a
indexedArrayCodec jiCodec = codec'
  (decoder arrayPrimCodec >>> decoder jiCodec)
  ( mkFn2 \extra a ->
      fst
        $ runFn2 (encoder arrayPrimCodec) extra
        $ Array.fromFoldable
        $ fst
        $ runFn2 (encoder jiCodec) extra a
  )

index :: forall e extra a. Int -> JsonCodec e extra a -> JIndexedCodec e extra a
index ix codec = Codec dec enc
  where
  dec = Decoder.do
    xs <- identity
    decodeIndex xs ix (decoder codec)
  enc = mkFn2 \extra a ->
    Tuple (List.singleton $ fst $ runFn2 (encoder codec) extra a) a

objectPrimCodec :: forall e extra. JsonCodec e extra (Object Json)
objectPrimCodec = mkJsonCodec decodeObjectPrim encodeObjectPrim

unitCodec :: forall e extra. JsonCodec e extra Unit
unitCodec = unit <$ nullCodec

intCodec :: forall e extra. JsonCodec e extra Int
intCodec = numberCodec >~> refinedValue
  (\n -> Int.fromNumber n # note ("Could not convert Number to Int: " <> show n))
  Int.toNumber

charCodec :: forall e extra. JsonCodec e extra Char
charCodec = stringCodec >~> refinedValue
  (\s -> charAt 0 s # note ("Could not get char at index 0 in String: " <> s))
  SCU.singleton

nonEmptyStringCodec :: forall e extra. JsonCodec e extra NonEmptyString
nonEmptyStringCodec =
  stringCodec >~> refinedValue
    (NonEmptyString.fromString >>> note "Received empty String")
    (\(NonEmptyString s) -> s)

arrayCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Array a)
arrayCodec aCodec =
  arrayPrimCodec >~> codec'
    ( Decoder.do
        arr <- identity
        forWithIndex arr \i j ->
          addOffset (AtIndex i) j (decoder aCodec)
    )
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

nonEmptyArrayCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (NonEmptyArray a)
nonEmptyArrayCodec aCodec =
  arrayCodec aCodec >~> refinedValue
    (NEA.fromArray >>> note "Received empty array")
    NEA.toArray

objectCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Object a)
objectCodec aCodec =
  objectPrimCodec >~> codec'
    ( Decoder.do
        obj <- identity
        forWithIndex obj \k j ->
          addOffset (AtKey k) j (decoder aCodec)
    )
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

nullableCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Nullable a)
nullableCodec aCodec =
  dimap Nullable.toMaybe Nullable.toNullable $ maybeNullableCodec aCodec

identityCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Identity a)
identityCodec = coerce

-- maybeTaggedCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
-- maybeTaggedCodec aCodec =

maybeNullableCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
maybeNullableCodec aCodec = codec'
  (altAccumulate (decoder (Nothing <$ nullCodec)) (decoder (Just <$> aCodec)))
  ( mkFn2 \extra a -> case a of
      Just a' -> fst $ runFn2 (encoder aCodec) extra a'
      Nothing -> fst $ runFn2 (encoder nullCodec) extra unit
  )
