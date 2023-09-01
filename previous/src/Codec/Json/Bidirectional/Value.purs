-- | Codecs in this module follow a naming convention:
-- | - the `j` prefix indicates a primitive JSON codec
-- |    (e.g. `null`, `number`, `boolean`, `string`, `array`, and `object`)
-- | - the type's name is used to refer to the codec for that type except in two cases:
-- |    - when the name clashes with a commonly used function/value, the `Codec` suffix is added:
-- |        - `identity` for `Identity` becomes `identityCodec`
-- |        - `void` for `Void` becomes `voidCodec`
-- |        - `map` for `Map` becomes `mapCodec`
-- |    - when a buildler-like API is needed, the `Prim` suffix is added
-- |        - `object` and `objectPrim`
-- |        - `record` and `recordPrim`
-- |        - `variant` and `variantPrim`
module Codec.Json.Bidirectional.Value
  ( json
  , voidCodec
  , jnull
  , unitCodec
  , boolean
  , number
  , int
  , char
  , codePoint
  , string
  , nonEmptyString
  , jarray
  , indexedArray
  , index
  , array
  , nonEmptyArray
  , jobject
  , objectPrim
  , fieldRequired
  , fieldOptional
  , object
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
  , either
  , tuple
  , these
  , nonEmpty
  , nonEmpty'
  , list
  , nonEmptyList
  , mapCodec
  , set
  , nonEmptySet
  , fix
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

import Codec.Codec (Codec(..), codec, codec', decoder, encoder, (>~>), (~))
import Codec.Decoder (altAccumulate)
import Codec.Decoder.Qualified as Decoder
import Codec.Json.Errors.DecodeMessages (arrayNotEmptyFailure, numToIntConversionFailure, stringNotEmptyFailure, stringToCharConversionFailure)
import Codec.Json.JsonCodec (JIndexedCodec, JPropCodec, JsonCodec, JsonCodec', addCtorHintC, addSubtermHintC, addTypeHintC, mkJsonCodec, refinedValue)
import Codec.Json.JsonDecoder (DecodeErrorAccumulatorFn, failWithStructureError, failWithUnrefinableValue)
import Codec.Json.Unidirectional.Decode.Value (decodeBoolean, decodeField, decodeField', decodeFields, decodeIndex, decodeIndices, decodeJArray, decodeJNull, decodeJObject, decodeNumber, decodeString, decodeVoid)
import Codec.Json.Unidirectional.Encode.Value (encodeJArray, encodeBoolean, encodeNumber, encodeJObject, encodeString, encodeUnitToNull, encodeVoid)
import Control.Monad.ST as ST
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, codePointAt)
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These(..))
import Data.Tuple (Tuple(..), fst, snd)
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

voidCodec :: forall e extra. JsonCodec e extra Void
voidCodec = mkJsonCodec decodeVoid encodeVoid

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
indexedArray jiCodec = jarray >~> codec'
  (decoder jiCodec)
  ( mkFn2 \extra a ->
      Array.fromFoldable
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
objectPrim fieldsCodec = jobject >~> codec'
  (decoder fieldsCodec)
  ( mkFn2 \extra a ->
      Object.fromFoldable
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
object aCodec = addTypeHintC "Object" do
  jobject >~> codec'
    (decodeFields (decoder aCodec))
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

unitCodec :: forall e extra. JsonCodec e extra Unit
unitCodec = addTypeHintC "Unit" do
  unit <$ jnull

int :: forall e extra. JsonCodec e extra Int
int = addTypeHintC "Int" do
  number >~> refinedValue
    (\n -> Int.fromNumber n # note (numToIntConversionFailure n))
    Int.toNumber

char :: forall e extra. JsonCodec e extra Char
char = addTypeHintC "Char" do
  string >~> refinedValue
    (\s -> charAt 0 s # note (stringToCharConversionFailure s))
    SCU.singleton

nonEmptyString :: forall e extra. JsonCodec e extra NonEmptyString
nonEmptyString = addTypeHintC "NonEmptyString" do
  string >~> refinedValue
    (NonEmptyString.fromString >>> note stringNotEmptyFailure)
    (NonEmptyString.toString)

codePoint :: forall e extra. JsonCodec e extra CodePoint
codePoint = addTypeHintC "CodePoint" do
  string >~> refinedValue (\s -> note ("Invalid code point for string: " <> show s) $ codePointAt 0 s) String.singleton

array :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Array a)
array aCodec = addTypeHintC "Array" do
  jarray >~> codec'
    (decodeIndices (decoder aCodec))
    ( mkFn2 \extra fa -> do
        let enc = encoder aCodec
        (\a -> fst $ runFn2 enc extra a) <$> fa
    )

nonEmptyArray :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (NonEmptyArray a)
nonEmptyArray aCodec = addTypeHintC "NonEmptyArray" do
  array aCodec >~> refinedValue
    (NEA.fromArray >>> note arrayNotEmptyFailure)
    NEA.toArray

-- | Codec for records where all fields are required and
-- | the JSON object's order of fields doesn't matter
-- | when using this codec to encode a value to JSON.
-- | If some fields are optional or the order matters,
-- | see `recordPrim`.
-- |
-- | ```
-- | record
-- |   { requiredFieldName1: pje $ string
-- |   , requiredFieldName2: pje $ array int
-- |   }
-- | ```
record
  :: forall e extra rl codecs to
   . RL.RowToList codecs rl
  => InsertRequiredPropCodecs e extra rl codecs () to
  => { | codecs }
  -> JsonCodec e extra { | to }
record codecs = recordPrim (requiredProps codecs)

-- | Codec builder for records where the field order matters
-- | when encoding JSON, and the underlying JSON object
-- | may contain optional fields. If order doesn't matter and
-- | all fields are required, see `record`.
-- |
-- | ```
-- | -- Order of fields is based on alphabetical ordering of labels
-- | recordPrim $
-- |   requiredProps
-- |     { requiredFieldName1: pje $ string
-- |     , requiredFieldName2: pje $ array int
-- |     }
-- |   >>> optionalProps
-- |     { optionalFieldName1: pje $ string
-- |     , optionalFieldName2: pje $ array int
-- |     }
-- |
-- | -- Order of fields is based on order below
-- | recordPrim $
-- |   requiredProp (Proxy :: _ "requiredFieldName1") (pje $ string)
-- |   >>> optionalProp (Proxy :: _ "optionalFieldName2") (pje $ array int)
-- |   >>> optionalProp (Proxy :: _ "optionalFieldName1") (pje $ string)
-- |   >>> requiredProp (Proxy :: _ "requiredFieldName2") (pje $ array int)
-- | ```
recordPrim :: forall e extra rows. (JPropCodec e extra {} -> JPropCodec e extra (Record rows)) -> JsonCodec e extra (Record rows)
recordPrim buildRecordCodec = addTypeHintC "Record" do
  jobject >~> codec'
    (decoder propCodec)
    ( mkFn2 \extra a ->
        Object.fromFoldable
          $ fst
          $ runFn2 (encoder propCodec) extra a
    )
  where
  propCodec = buildRecordCodec recordEmpty

  recordEmpty :: JPropCodec e extra {}
  recordEmpty = Codec (pure {}) (mkFn2 \_ a -> Tuple Nil a)

-- | See `recordPrim`
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

-- | See `recordPrim`
requiredProps
  :: forall e extra rl codecs from to
   . RL.RowToList codecs rl
  => InsertRequiredPropCodecs e extra rl codecs from to
  => { | codecs }
  -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })
requiredProps codecs =
  insertRequiredPropCodecs (RlJCodec codecs :: RlJCodec e extra rl codecs)

-- | See `recordPrim`
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

-- | See `recordPrim`
optionalProps
  :: forall e extra rl codecs from to
   . RL.RowToList codecs rl
  => InsertOptionalPropCodecs e extra rl codecs from to
  => { | codecs }
  -> (JPropCodec e extra { | from } -> JPropCodec e extra { | to })
optionalProps codecs =
  insertOptionalPropCodecs (RlJCodec codecs :: RlJCodec e extra rl codecs)

-- | Codec for `Variant` where the order of the handlers
-- | (e.g. which tag is checked first) does not matter.
-- | If that is not the case, see `variantPrim`.
-- |
-- | ```
-- | variant
-- |   { tag1: pje $ string
-- |   , tag2: pje $ array int
-- |   }
-- | ```
variant
  :: forall e extra rows rl out
   . RL.RowToList rows rl
  => VariantJsonCodec e extra rl rows out
  => DecodeErrorAccumulatorFn e extra (Object Json) (Variant out)
  -> { | rows }
  -> JsonCodec e extra (Variant out)
variant errAcc r = variantPrim errAcc (variantJsonCodec (RlRecord r :: RlRecord e extra rl rows))

-- | This is probably not what you want. See `variantPrim`.
-- | This is only exported if one needs it.
variantEmpty :: forall e extra from. JsonCodec' e extra from (Variant ())
variantEmpty = codec' (failWithUnrefinableValue "All variant decoders failed to decode") (mkFn2 \_ v -> case_ v)

-- | Codec for `Variant` where the order of the handlers
-- | (e.g. which tag is checked first) DOES matter.
-- | If that is not the case and you want simpler syntax,
-- | see `variant`.
-- |
-- | ```
-- | variantPrim $
-- |   variantCase (Proxy :: _ "tag2") (Tuple (addCtorHintC "SomeCtorWithArg") $ Right $ pje $ array int)
-- |   >>> variantCase (Proxy :: _ "tag1") (Tuple (addCtorHintC "SomeCtorNoArg") $ Left unit)
-- | ```
variantPrim
  :: forall e extra rows
   . DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows)
  -> ( ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant ())
         -> JsonCodec' e extra (Object Json) (Variant ())
       )
       -> ( DecodeErrorAccumulatorFn e extra (Object Json) (Variant rows)
            -> JsonCodec' e extra (Object Json) (Variant rows)
          )
     )
  -> JsonCodec e extra (Variant rows)
variantPrim accErrs buildCodec = addTypeHintC "Variant" do
  jobject >~> (buildCodec (\_ -> variantEmpty) accErrs)

-- | See `variantPrim`.
variantCase
  :: forall e extra sym a tail row
   . IsSymbol sym
  => Row.Cons sym a tail row
  => Proxy sym
  -> Tuple (JsonCodec' e extra (Object Json) (Variant row) -> JsonCodec' e extra (Object Json) (Variant row)) (Either a (JsonCodec e extra a))
  -> ( (DecodeErrorAccumulatorFn e extra (Object Json) (Variant tail) -> JsonCodec' e extra (Object Json) (Variant tail))
       -> (DecodeErrorAccumulatorFn e extra (Object Json) (Variant row) -> JsonCodec' e extra (Object Json) (Variant row))
     )
variantCase _sym (Tuple addHint eacodec) = \buildTailCodec errorAccumulator -> do
  let
    Codec dec enc = buildTailCodec $ coerceA errorAccumulator
  addHint $ codec'
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
nullable aCodec = addTypeHintC "Nullable" do
  codec'
    (altAccumulate (decoder (Nullable.toNullable Nothing <$ jnull)) (decoder (Nullable.toNullable <<< Just <$> aCodec)))
    ( mkFn2 \extra a -> case Nullable.toMaybe a of
        Just a' -> fst $ runFn2 (encoder aCodec) extra a'
        Nothing -> fst $ runFn2 (encoder jnull) extra unit
    )

identityCodec :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Identity a)
identityCodec = addTypeHintC "Identity" <<< coerce

maybe :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (Maybe a)
maybe codecA = addTypeHintC "Maybe" do
  dimap toVariant fromVariant
    $ variantPrim altAccumulate
    $ variantCase _just (Tuple (addCtorHintC "Just") $ Right codecA)
        >>> variantCase _nothing (Tuple (addCtorHintC "Nothing") $ Left unit)
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

either :: forall e extra a b. JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra (Either a b)
either codecA codecB = addTypeHintC "Either" do
  dimap toVariant fromVariant
    $ variantPrim altAccumulate
    $ variantCase _left (Tuple (addCtorHintC "Left") $ Right codecA)
        >>> variantCase _right (Tuple (addCtorHintC "Right") $ Right codecB)
  where
  _left = Proxy :: Proxy "Left"
  _right = Proxy :: Proxy "Right"

  toVariant = case _ of
    Left a -> V.inj _left a
    Right b -> V.inj _right b
  fromVariant = V.match
    { "Left": Left
    , "Right": Right
    }

tuple :: forall e extra a b. JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra (Tuple a b)
tuple codecA codecB = addTypeHintC "Tuple" do
  indexedArray $
    Tuple
      <$> (addSubtermHintC 0 $ fst ~ index 0 codecA)
      <*> (addSubtermHintC 1 $ snd ~ index 1 codecB)

these :: forall e extra a b. JsonCodec e extra a -> JsonCodec e extra b -> JsonCodec e extra (These a b)
these codecA codecB = addTypeHintC "These" do
  dimap toVariant fromVariant
    $ variantPrim altAccumulate
    $ variantCase _this (Tuple (addCtorHintC "This") $ Right codecA)
        >>> variantCase _that (Tuple (addCtorHintC "That") $ Right codecB)
        >>> variantCase _both
          ( Tuple (addCtorHintC "Both") $ Right $ record
              { this: addSubtermHintC 0 codecA
              , that: addSubtermHintC 1 codecB
              }
          )
  where
  _this = Proxy :: Proxy "This"
  _that = Proxy :: Proxy "That"
  _both = Proxy :: Proxy "Both"

  toVariant = case _ of
    This a -> V.inj _this a
    That b -> V.inj _that b
    Both a b -> V.inj _both { this: a, that: b }
  fromVariant = V.match
    { "This": This
    , "That": That
    , "Both": \{ this, that } -> Both this that
    }

-- | Slightly simpler version of `nonEmpty'`. Rather than writing,
-- | `nonEmpty' int (array int)`, one can use this function and just write
-- | `nonEmpty int array`.
nonEmpty :: forall e extra f a. JsonCodec e extra a -> (JsonCodec e extra a -> JsonCodec e extra (f a)) -> JsonCodec e extra (NonEmpty f a)
nonEmpty codecA codecF = nonEmpty' codecA (codecF codecA)

-- | Slightly more verbose version of `nonEmpty`.
nonEmpty' :: forall e extra f a. JsonCodec e extra a -> JsonCodec e extra (f a) -> JsonCodec e extra (NonEmpty f a)
nonEmpty' codecA codecFA = addTypeHintC "NonEmpty" do
  dimap to from $
    record
      { head: addSubtermHintC 0 codecA
      , tail: addSubtermHintC 1 codecFA
      }
  where
  to (NonEmpty a fa) = { head: a, tail: fa }
  from { head, tail } = NonEmpty head tail

list :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (List a)
list codecA = addTypeHintC "List" do
  dimap Array.fromFoldable List.fromFoldable
    $ array codecA

nonEmptyList :: forall e extra a. JsonCodec e extra a -> JsonCodec e extra (NonEmptyList a)
nonEmptyList codecA = addTypeHintC "NonEmptyList" do
  list codecA >~> refinedValue (note "Received empty list" <<< NEL.fromList) NEL.toList

mapCodec :: forall e extra k v. Ord k => JsonCodec e extra k -> JsonCodec e extra v -> JsonCodec e extra (Map k v)
mapCodec codecKey codecValue = addTypeHintC "Map" do
  dimap to from
    $ array (record { key: codecKey, value: codecValue })
  where
  to = foldlWithIndex (\key acc value -> Array.snoc acc { key, value }) []
  from = Array.foldl (\m { key, value } -> Map.insert key value m) Map.empty

set :: forall e extra a. Ord a => JsonCodec e extra a -> JsonCodec e extra (Set a)
set codecValue = addTypeHintC "Set" do
  dimap to from $ array codecValue
  where
  to = Set.toUnfoldable
  from = Array.foldl (flip Set.insert) Set.empty

nonEmptySet :: forall e extra a. Ord a => JsonCodec e extra a -> JsonCodec e extra (NonEmptySet a)
nonEmptySet codecValue = addTypeHintC "NonEmptySet" do
  set codecValue >~> refinedValue (note "Received empty set" <<< NonEmptySet.fromSet) (NonEmptySet.toSet)

fix :: forall e extra a. (JsonCodec e extra a -> JsonCodec e extra a) -> JsonCodec e extra a
fix f = codec'
  (decoder $ f (fix f))
  (mkFn2 \extra a -> fst $ runFn2 (encoder $ f (fix f)) extra a)

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
  ( Row.Cons sym (Tuple (JsonCodec' e extra (Object Json) (Variant out) -> JsonCodec' e extra (Object Json) (Variant out)) (Either a (JsonCodec e extra a))) codecRows' codecRows
  , VariantJsonCodec e extra tail codecRows' out'
  , Row.Cons sym a out' out
  , IsSymbol sym
  ) =>
  VariantJsonCodec e extra (RL.Cons sym (Tuple (JsonCodec' e extra (Object Json) (Variant out) -> JsonCodec' e extra (Object Json) (Variant out)) (Either a (JsonCodec e extra a))) tail) codecRows out where
  variantJsonCodec (RlRecord r) =
    (variantJsonCodec (RlRecord $ unsafeForget r :: RlRecord e extra tail codecRows'))
      >>> variantCase _sym (Record.get _sym r)
    where
    _sym = Proxy :: Proxy sym

    unsafeForget :: { | codecRows } -> { | codecRows' }
    unsafeForget = unsafeCoerce
