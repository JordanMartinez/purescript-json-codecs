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
  , variant
  , variantPrim
  , variantCase
  , nullable
  , identityCodec
  , maybe
  , RlJCodec
  , class InsertRequiredPropCodecs
  , insertRequiredPropCodecs
  , class InsertOptionalPropCodecs
  , insertOptionalPropCodecs
  , RlRecord
  , class VariantJsonCodec
  , variantJsonCodec
  ) where

import Prelude

import Codec.Codec (Codec(..), codec, codec', decoder, encoder, (>~>))
import Codec.Decoder (altAccumulate)
import Codec.Decoder.Qualified as Decoder
import Codec.Json.Errors.DecodeMessages (arrayNotEmptyFailure, numToIntConversionFailure, stringNotEmptyFailure, stringToCharConversionFailure)
import Codec.Json.JsonCodec (JIndexedCodec, JPropCodec, JsonCodec', JsonCodec, mkJsonCodec, refinedValue)
import Codec.Json.JsonDecoder (DecodeErrorAccumulatorFn, addOffset, failWithStructureError, failWithUnrefinableValue)
import Codec.Json.Types (JsonOffset(..))
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeField, decodeField', decodeIndex, decodeJArray, decodeJNull, decodeJObject, decodeNumber, decodeString, decodeVoid)
import Codec.Json.Unidirectional.Encode.Value (encodeJArray, encodeBoolean, encodeNumber, encodeJObject, encodeString, encodeUnitToNull, encodeVoid)
import Control.Monad.ST as ST
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
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
import Data.Variant (Variant, case_)
import Data.Variant as V
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST as FOST
import Foreign.Object.ST.Unsafe as FOSTU
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
  dec = ado
    r <- decoder codecR
    a <- decodeField key (decoder codecA)
    in unsafeSet key a r

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
  dec = ado
    r <- decoder codecR
    a <- decodeField' key (pure Nothing) (Just <$> decoder codecA)
    in unsafeSet key a r

  enc :: Fn2 extra { | r' } (Tuple (List (Tuple String Json)) { | r' })
  enc = mkFn2 \extra val -> do
    let tail = fst $ runFn2 (encoder codecR) extra (unsafeForget val)
    let mbHead = map (\a -> Tuple key (fst $ runFn2 (encoder codecA) extra a)) $ unsafeGet key val
    Tuple (Maybe.maybe tail (\h -> h : tail) mbHead) val

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

variant
  :: forall e extra rows rl out
   . RL.RowToList rows rl
  => VariantJsonCodec e extra rl rows out
  => DecodeErrorAccumulatorFn e extra (Object Json) (Variant out)
  -> { | rows }
  -> JsonCodec e extra (Variant out)
variant errAcc r = variantPrim errAcc (variantJsonCodec (RlRecord r :: RlRecord e extra rl rows))

variantEmpty :: forall e extra from. JsonCodec' e extra from (Variant ())
variantEmpty = codec' (failWithUnrefinableValue "All variant decoders failed to decode") (mkFn2 \_ v -> case_ v)

variantPrim
  :: forall e extra rows
   . DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows)
  -> ( ((DecodeErrorAccumulatorFn e extra (Object Json) (Variant ()) -> JsonCodec' e extra (Object Json) (Variant ())))
       -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows) -> JsonCodec' e extra (Object Json) (Variant rows))
     )
  -> JsonCodec e extra (Variant rows)
variantPrim accErrs buildCodec = jobject >~> (buildCodec (\_ -> variantEmpty) accErrs)

variantCase
  :: forall e extra sym a tail row
   . IsSymbol sym
  => Row.Cons sym a tail row
  => Proxy sym
  -> Either a (JsonCodec e extra a)
  -> ( (DecodeErrorAccumulatorFn e extra (Object Json) (Variant tail) -> JsonCodec' e extra (Object Json) (Variant tail))
       -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant row) -> JsonCodec' e extra (Object Json) (Variant row))
     )
variantCase _sym eacodec = \buildTailCodec errorAccumulator -> do
  let
    Codec dec enc = buildTailCodec $ coerceA errorAccumulator
  codec'
    ( Decoder.do
        tag <- decodeField "tag" (decoder string)
        if tag == label then
          case eacodec of
            Left a -> pure (V.inj _sym a)
            Right codec -> V.inj _sym <$> decodeField "value" (decoder codec)
        else
          errorAccumulator
            (failWithStructureError $ "Did not get expected tag, " <> show label)
            (coerceR <$> dec)
    )
    ( mkFn2 \extra v ->
        V.on _sym
          ( \v' -> ST.run do
              obj <- FOST.new
              _ <- FOST.poke "tag" (fst $ runFn2 (encoder string) extra label) obj
              _ <- case eacodec of
                Left _ → pure obj
                Right codec → FOST.poke "value" (fst $ runFn2 (encoder codec) extra v') obj
              FOSTU.unsafeFreeze obj
          )
          (\v' -> fst $ runFn2 enc extra v')
          v
    )
  where
  label = reflectSymbol _sym

  coerceR :: Variant tail -> Variant row
  coerceR = unsafeCoerce

  coerceA
    :: DecodeErrorAccumulatorFn e extra (Object Json) (Variant row)
    -> DecodeErrorAccumulatorFn e extra (Object Json) (Variant tail)
  coerceA = unsafeCoerce

nullable :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Nullable a)
nullable aCodec = codec'
  (altAccumulate (decoder (Nullable.toNullable Nothing <$ jnull)) (decoder (Nullable.toNullable <<< Just <$> aCodec)))
  ( mkFn2 \extra a -> case Nullable.toMaybe a of
      Just a' -> fst $ runFn2 (encoder aCodec) extra a'
      Nothing -> fst $ runFn2 (encoder jnull) extra unit
  )

identityCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Identity a)
identityCodec = coerce

maybe :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
maybe codecA = dimap toVariant fromVariant
  $ variantPrim altAccumulate
  $ variantCase _just (Right codecA)
      >>> variantCase _nothing (Left unit)
  where
  _just = Proxy :: Proxy "Just"
  _nothing = Proxy :: Proxy "Nothing"

  toVariant = case _ of
    Just a -> V.inj _just a
    Nothing -> V.inj _nothing unit
  fromVariant = V.match
    { "Just": Just
    , "Nothing": const Nothing
    }

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

newtype RlRecord :: Type -> Type -> RL.RowList Type -> Row Type -> Type
newtype RlRecord e extra rowlist rows = RlRecord { | rows }

class VariantJsonCodec :: Type -> Type -> RL.RowList Type -> Row Type -> Row Type -> Constraint
class VariantJsonCodec e extra rowlist row out | e extra rowlist -> row out where
  variantJsonCodec
    :: RlRecord e extra rowlist row
    -> ( (DecodeErrorAccumulatorFn e extra (Object Json) (Variant ()) -> JsonCodec' e extra (Object Json) (Variant ()))
         -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant out) -> JsonCodec' e extra (Object Json) (Variant out))
       )

instance variantJsonCodecNil :: VariantJsonCodec e extra RL.Nil () () where
  variantJsonCodec _ = \buildTailCodec errorAccumulator -> buildTailCodec errorAccumulator

instance variantJsonCodecCons ::
  ( Row.Cons sym (Either a (JsonCodec e extra a)) codecRows' codecRows
  , VariantJsonCodec e extra tail codecRows' out'
  , Row.Cons sym a out' out
  , IsSymbol sym
  ) =>
  VariantJsonCodec e extra (RL.Cons sym (Either a (JsonCodec e extra a)) tail) codecRows out where
  variantJsonCodec (RlRecord r) =
    (variantJsonCodec (RlRecord $ unsafeForget r :: RlRecord e extra tail codecRows'))
      >>> variantCase _sym (Record.get _sym r)
    where
    _sym = Proxy :: Proxy sym

    unsafeForget :: { | codecRows } -> { | codecRows' }
    unsafeForget = unsafeCoerce
