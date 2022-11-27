module Codec.Json.Bidirectional.Value
  ( json
  , void
  , jnull
  , boolean
  , number
  , string
  , jarray
  , indexedArray
  , index
  , jobject
  , objectPrim
  , fieldRequired
  , fieldOptional
  , object
  , unitCodec
  , int
  , char
  , nonEmptyString
  , array
  , nonEmptyArray
  , record
  , recordPrim
  , requiredProp
  , requiredProps
  , optionalProp
  , optionalProps
  , nullable
  , identityCodec
  , RlJCodec
  , class InsertRequiredPropCodecs
  , insertRequiredPropCodecs
  , class InsertOptionalPropCodecs
  , insertOptionalPropCodecs
  ) where

import Prelude

import Codec.Codec (Codec(..), codec, codec', decoder, encoder, (>~>))
import Codec.Decoder (altAccumulate)
import Codec.Decoder.Qualified as Decoder
import Codec.Json.Errors.DecodeMessages (arrayNotEmptyFailure, numToIntConversionFailure, stringNotEmptyFailure, stringToCharConversionFailure)
import Codec.Json.JsonCodec (JIndexedCodec, JPropCodec, JsonCodec, mkJsonCodec, refinedValue)
import Codec.Json.JsonDecoder (addOffset)
import Codec.Json.Types (JsonOffset(..))
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeField, decodeField', decodeIndex, decodeJArray, decodeJNull, decodeJObject, decodeNumber, decodeString, decodeVoid)
import Codec.Json.Unidirectional.Encode.Value (encodeJArray, encodeBoolean, encodeNumber, encodeJObject, encodeString, encodeUnitToNull, encodeVoid)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (note)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor (dimap)
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Unsafe (unsafeGet, unsafeSet)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
  dec = decodeIndex ix (decoder codec)
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

fieldRequired :: forall e extra a. String -> JsonCodec e extra a -> JPropCodec e extra a
fieldRequired key propCodec = codec
  (decodeField key (decoder propCodec))
  ( mkFn2 \extra a ->
      pure $ Tuple key $ fst $ runFn2 (encoder propCodec) extra a
  )

fieldOptional :: forall e extra a. String -> JsonCodec e extra a -> JPropCodec e extra (Maybe a)
fieldOptional key propCodec = codec
  (decodeField' key (pure Nothing) (Just <$> decoder propCodec))
  ( mkFn2 \extra a -> case a of
      Nothing -> Nil
      Just a' -> pure $ Tuple key $ fst $ runFn2 (encoder propCodec) extra a'
  )

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

unitCodec :: forall e extra. JsonCodec e extra Unit
unitCodec = unit <$ jnull

int :: forall e extra. JsonCodec e extra Int
int = number >~> refinedValue
  (\n -> Int.fromNumber n # note (numToIntConversionFailure n))
  Int.toNumber

char :: forall e extra. JsonCodec e extra Char
char = string >~> refinedValue
  (\s -> charAt 0 s # note (stringToCharConversionFailure s))
  SCU.singleton

nonEmptyString :: forall e extra. JsonCodec e extra NonEmptyString
nonEmptyString =
  string >~> refinedValue
    (NonEmptyString.fromString >>> note stringNotEmptyFailure)
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
    (NEA.fromArray >>> note arrayNotEmptyFailure)
    NEA.toArray

record
  :: forall e extra rl codecs to
   . RL.RowToList codecs rl
  => InsertRequiredPropCodecs e extra rl codecs () to
  => { | codecs }
  -> JsonCodec e extra { | to }
record codecs = recordPrim (requiredProps codecs)

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
  propCodec = buildRecordCodec recordEmpty

  recordEmpty :: JPropCodec e extra {}
  recordEmpty = Codec (pure {}) (mkFn2 \_ a -> Tuple Nil a)

requiredProp
  :: forall e extra sym a r r'
   . IsSymbol sym
  => Row.Cons sym a r r'
  => Proxy sym
  -> JsonCodec e extra a
  -> JPropCodec e extra { | r }
  -> JPropCodec e extra { | r' }
requiredProp _sym codecA codecR = Codec dec enc
  where
  key = reflectSymbol _sym
  dec = Decoder.do
    r <- decoder codecR
    a <- decodeField key (decoder codecA)
    pure $ unsafeSet key a r

  enc :: Fn2 extra { | r' } (Tuple (List (Tuple String Json)) { | r' })
  enc = mkFn2 \extra val ->
    Tuple
      ( Tuple key (fst $ runFn2 (encoder codecA) extra $ unsafeGet key val)
          : (fst $ runFn2 (encoder codecR) extra (unsafeForget val))
      )
      val

  unsafeForget :: Record r' → Record r
  unsafeForget = unsafeCoerce

requiredProps
  :: forall e extra rl codecs from to
   . RL.RowToList codecs rl
  => InsertRequiredPropCodecs e extra rl codecs from to
  => { | codecs }
  -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })
requiredProps codecs =
  insertRequiredPropCodecs (RlJCodec codecs :: RlJCodec e extra rl codecs)

optionalProp
  :: forall e extra sym a r r'
   . IsSymbol sym
  => Row.Cons sym (Maybe a) r r'
  => Proxy sym
  -> JsonCodec e extra a
  -> JPropCodec e extra { | r }
  -> JPropCodec e extra { | r' }
optionalProp _sym codecA codecR = Codec dec enc
  where
  key = reflectSymbol _sym
  dec = Decoder.do
    r <- decoder codecR
    a <- decodeField' key (pure Nothing) (Just <$> decoder codecA)
    pure $ unsafeSet key a r

  enc :: Fn2 extra { | r' } (Tuple (List (Tuple String Json)) { | r' })
  enc = mkFn2 \extra val -> do
    let tail = fst $ runFn2 (encoder codecR) extra (unsafeForget val)
    let mbHead = map (\a -> Tuple key (fst $ runFn2 (encoder codecA) extra a)) $ unsafeGet key val
    Tuple (maybe tail (\h -> h : tail) mbHead) val

  unsafeForget :: Record r' → Record r
  unsafeForget = unsafeCoerce

optionalProps
  :: forall e extra rl codecs from to
   . RL.RowToList codecs rl
  => InsertOptionalPropCodecs e extra rl codecs from to
  => { | codecs }
  -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })
optionalProps codecs =
  insertOptionalPropCodecs (RlJCodec codecs :: RlJCodec e extra rl codecs)

nullable :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Nullable a)
nullable aCodec = codec'
  (altAccumulate (decoder (Nullable.toNullable Nothing <$ jnull)) (decoder (Nullable.toNullable <<< Just <$> aCodec)))
  ( mkFn2 \extra a -> case Nullable.toMaybe a of
      Just a' -> fst $ runFn2 (encoder aCodec) extra a'
      Nothing -> fst $ runFn2 (encoder jnull) extra unit
  )

identityCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Identity a)
identityCodec = coerce

-- maybeTagged :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
-- maybeTaggedCodec a =

newtype RlJCodec :: Type -> Type -> RL.RowList Type -> Row Type -> Type
newtype RlJCodec e extra rl row = RlJCodec { | row }

class InsertRequiredPropCodecs :: Type -> Type -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class InsertRequiredPropCodecs e extra rl codecs from to | e extra rl -> codecs from to where
  insertRequiredPropCodecs :: RlJCodec e extra rl codecs -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })

instance insertRequiredPropCodecsNil :: InsertRequiredPropCodecs e extra RL.Nil () same same where
  insertRequiredPropCodecs _ = identity

instance insertRequiredPropCodecsCons ::
  ( Row.Cons sym (JsonCodec e extra a) codecRows' codecRows
  , InsertRequiredPropCodecs e extra tail codecRows' start from
  , Row.Cons sym a from to
  , IsSymbol sym
  ) =>
  InsertRequiredPropCodecs e extra (RL.Cons sym (JsonCodec e extra a) tail) codecRows start to where
  insertRequiredPropCodecs (RlJCodec r) from =
    requiredProp _sym (Record.get _sym r)
      $ insertRequiredPropCodecs (RlJCodec $ unsafeForget r :: RlJCodec e extra tail codecRows') from
    where
    _sym = Proxy :: Proxy sym

    unsafeForget :: { | codecRows } -> { | codecRows' }
    unsafeForget = unsafeCoerce

class InsertOptionalPropCodecs :: Type -> Type -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class InsertOptionalPropCodecs e extra rl codecs from to | e extra rl -> codecs from to where
  insertOptionalPropCodecs :: RlJCodec e extra rl codecs -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })

instance insertOptionalPropCodecsNil :: InsertOptionalPropCodecs e extra RL.Nil () same same where
  insertOptionalPropCodecs _ = identity

instance insertOptionalPropCodecsCons ::
  ( Row.Cons sym (JsonCodec e extra a) codecRows' codecRows
  , InsertOptionalPropCodecs e extra tail codecRows' start from
  , Row.Cons sym (Maybe a) from to
  , IsSymbol sym
  ) =>
  InsertOptionalPropCodecs e extra (RL.Cons sym (JsonCodec e extra a) tail) codecRows start to where
  insertOptionalPropCodecs (RlJCodec r) from =
    optionalProp _sym (Record.get _sym r)
      $ insertOptionalPropCodecs (RlJCodec $ unsafeForget r :: RlJCodec e extra tail codecRows') from
    where
    _sym = Proxy :: Proxy sym

    unsafeForget :: { | codecRows } -> { | codecRows' }
    unsafeForget = unsafeCoerce
