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
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor (dimap)
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Json.JsonCodec (JIndexedCodec, JsonCodec, JPropCodec, mkJsonCodec, refinedValue)
import Json.JsonDecoder (addOffset)
import Json.Types (JsonOffset(..))
import Json.Unidirectional.Decode.Value (decodeJArray, decodeBoolean, decodeField, decodeIndex, decodeJNull, decodeNumber, decodeJObject, decodeString, decodeVoid)
import Json.Unidirectional.Encode.Value (encodeJArray, encodeBoolean, encodeNumber, encodeJObject, encodeString, encodeUnitToNull, encodeVoid)
import Prim.Row as Row
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

json :: forall e extra. JsonCodec e extra Json
json = mkJsonCodec identity identity

void :: forall e extra. JsonCodec e extra Void
void = mkJsonCodec decodeVoid encodeVoid

jnull :: forall e extra. JsonCodec e extra Unit
jnull = mkJsonCodec decodeJNull encodeUnitToNull

boolean :: forall e extra. JsonCodec e extra Boolean
boolean = mkJsonCodec decodeBoolean encodeBoolean

number :: forall e extra. JsonCodec e extra Number
number = mkJsonCodec decodeNumber encodeNumber

string :: forall e extra. JsonCodec e extra String
string = mkJsonCodec decodeString encodeString

jarray :: forall e extra. JsonCodec e extra (Array Json)
jarray = mkJsonCodec decodeJArray encodeJArray

indexedArray :: forall e extra a. JIndexedCodec e extra a -> JsonCodec e extra a
indexedArray jiCodec = codec'
  (decoder jarray >>> decoder jiCodec)
  ( mkFn2 \extra a ->
      fst
        $ runFn2 (encoder jarray) extra
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

jobject :: forall e extra. JsonCodec e extra (Object Json)
jobject = mkJsonCodec decodeJObject encodeJObject

objectPrim :: forall e extra a. JPropCodec e extra a -> JsonCodec e extra a
objectPrim fieldsCodec = codec'
  (decoder jobject >>> decoder fieldsCodec)
  ( mkFn2 \extra a ->
      fst
        $ runFn2 (encoder jobject) extra
        $ Object.fromFoldable
        $ fst
        $ runFn2 (encoder fieldsCodec) extra a
  )

requiredField :: forall e extra a. String -> JsonCodec e extra a -> JPropCodec e extra a
requiredField key propCodec = codec
  ( Decoder.do
      obj <- identity
      decodeField obj key (decoder propCodec)
  )
  ( mkFn2 \extra a ->
      pure $ Tuple key $ fst $ runFn2 (encoder propCodec) extra a
  )

recordPrim :: forall e extra a. (JPropCodec e extra {} -> JPropCodec e extra a) -> JsonCodec e extra a
recordPrim buildRecordCodec = codec'
  (decoder jobject >>> decoder propCodec)
  ( mkFn2 \extra a ->
      fst
        $ runFn2 (encoder jobject) extra
        $ Object.fromFoldable
        $ fst
        $ runFn2 (encoder propCodec) extra a
  )
  where
  propCodec = buildRecordCodec start

  start :: JPropCodec e extra {}
  start = Codec (pure {}) (mkFn2 \_ a -> Tuple Nil a)

-- propRequired
--   :: forall e extra sym a r r'
--    . IsSymbol sym
--   => Row.Cons sym a r r'
--   => Proxy sym
--   -> JsonCodec e extra a
--   -> JPropCodec e extra { | r }
--   -> JPropCodec e extra { | r' }
-- propRequired _sym codecA codecR = ?help

-- propOptional
--   :: forall e extra sym a r r'
--    . IsSymbol sym
--   => Row.Cons sym (Maybe a) r r'
--   => Proxy sym
--   -> JsonCodec e extra a
--   -> JPropCodec e extra { | r }
--   -> JPropCodec e extra { | r' }
-- propOptional p codecA codecR = ?help

unitCodec :: forall e extra. JsonCodec e extra Unit
unitCodec = unit <$ jnull

int :: forall e extra. JsonCodec e extra Int
int = number >~> refinedValue
  (\n -> Int.fromNumber n # note ("Could not convert Number to Int: " <> show n))
  Int.toNumber

char :: forall e extra. JsonCodec e extra Char
char = string >~> refinedValue
  (\s -> charAt 0 s # note ("Could not get char at index 0 in String: " <> s))
  SCU.singleton

nonEmptyString :: forall e extra. JsonCodec e extra NonEmptyString
nonEmptyString =
  string >~> refinedValue
    (NonEmptyString.fromString >>> note "Received empty String")
    (\(NonEmptyString s) -> s)

array :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Array a)
array aCodec =
  jarray >~> codec'
    ( Decoder.do
        arr <- identity
        forWithIndex arr \i j ->
          addOffset (AtIndex i) j (decoder aCodec)
    )
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

nonEmptyArray :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (NonEmptyArray a)
nonEmptyArray aCodec =
  array aCodec >~> refinedValue
    (NEA.fromArray >>> note "Received empty array")
    NEA.toArray

object :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Object a)
object aCodec =
  jobject >~> codec'
    ( Decoder.do
        obj <- identity
        forWithIndex obj \k j ->
          addOffset (AtKey k) j (decoder aCodec)
    )
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

nullable :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Nullable a)
nullable aCodec =
  dimap Nullable.toMaybe Nullable.toNullable $ maybeNullable aCodec

identityCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Identity a)
identityCodec = coerce

-- maybeTagged :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
-- maybeTaggedCodec a =

maybeNullable :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
maybeNullable aCodec = codec'
  (altAccumulate (decoder (Nothing <$ jnull)) (decoder (Just <$> aCodec)))
  ( mkFn2 \extra a -> case a of
      Just a' -> fst $ runFn2 (encoder aCodec) extra a'
      Nothing -> fst $ runFn2 (encoder jnull) extra unit
  )
