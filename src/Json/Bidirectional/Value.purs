module Json.Bidirectional.Value where

import Prelude

import Codec.Codec ((>~>))
import Data.Argonaut.Core (Json)
import Data.Either (note)
import Data.Int as Int
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Foreign.Object (Object)
import Json.JsonCodec (JsonCodec, mkJsonCodec, refinedValue)
import Json.Unidirectional.Decode.Value (decodeArrayPrim, decodeBoolean, decodeNull, decodeNumber, decodeObjectPrim, decodeString, decodeVoid)
import Json.Unidirectional.Encode.Value (encodeArrayPrim, encodeBoolean, encodeNumber, encodeObjectPrim, encodeString, encodeUnitToNull, encodeVoid)

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

-- -- Issue here is with decoder: `e` is not a Semigroup, so can't use `forWithIndex`
-- arrayCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Array a)
-- arrayCodec aCodec =
--   arrayPrimCodec >~> codec'
--     -- ( DecoderFn $ mkFn5 \path appendFn handlers@(JsonErrorHandlers h) extra fJson ->
--     --     forWithIndex fJson \i j ->
--     --       runFn5 (unwrap $ decoder aCodec) (if h.includeJsonOffset then Array.snoc path (AtIndex i) else path) appendFn handlers extra j
--     -- )
--     (?help)
--     ( mkFn2 \extra fa -> do
--         let enc = encoder aCodec
--         (\a -> fst $ runFn2 enc extra a) <$> fa
--     )

objectPrimCodec :: forall e extra. JsonCodec e extra (Object Json)
objectPrimCodec = mkJsonCodec decodeObjectPrim encodeObjectPrim

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
