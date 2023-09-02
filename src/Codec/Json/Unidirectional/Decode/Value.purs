module Codec.Json.Unidirectional.Decode.Value
  ( refine
  , coerce1
  , toVoid
  , toJNull
  , toNullDefaultOrA
  , toNullNothingOrJust
  , toNullable
  , toBoolean
  , toNumber
  , toInt
  , toString
  , toChar
  , toNonEmptyString
  , toJArray
  , underIndex
  , underIndex'
  , toArray
  , toArray2
  , toArray3
  , toArray4
  , toArray5
  , toNonEmptyArray
  , toJObject
  , underKey
  , underKey'
  , toObject
  , toIdentity
  , toMaybeTagged
  , toEither
  , toTuple
  , toThese
  , toNonEmpty
  , toList
  , toNonEmptyList
  , toMap
  , toSet
  , toNonEmptySet
  , toCodePoint
  , ToRecordCodec(..)
  , toRecord
  , toRecordN
  , toStatic
  , toRequired
  , toRequiredRename
  , toOption
  , toOptionRename
  , toOptionDefault
  , toOptionDefaultRename
  , toOptionArray
  , toOptionAssocArray
  , class ToRecordObj
  , toRecordObj
  ) where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder, addCtorHint, addSubtermHint, addTypeHint, altAccumulateLazy, onMissingField, onMissingIndex, onStructureError, onTypeMismatch, onUnrefinableValue, withAttempts, withIndex, withKey, withSubtermHint, withTypeHint)
import Data.Argonaut.Core (Json, caseJson)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), note)
import Data.Function.Uncurried (runFn2)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, notNull, null)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, codePointAt)
import Data.String.CodeUnits (charAt)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol)
import Data.These (These(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | ```
-- | import Data.String.NonEmpty as NES
-- |
-- | toRefined (note "Received empty string" <<< NES.fromString) toString
-- | ```
refine
  :: forall @f a b
   . IsJsonDecoder f
  => (a -> Either String b)
  -> a
  -> f b
refine f a = case f a of
  Left e -> onUnrefinableValue e
  Right b -> pure b

toJNull :: forall @f. IsJsonDecoder f => Json -> f Unit
toJNull json =
  caseJson
    pure
    (\_ -> runFn2 onTypeMismatch "Null" "Boolean")
    (\_ -> runFn2 onTypeMismatch "Null" "Number")
    (\_ -> runFn2 onTypeMismatch "Null" "String")
    (\_ -> runFn2 onTypeMismatch "Null" "Array")
    (\_ -> runFn2 onTypeMismatch "Null" "Object")
    json

toNullDefaultOrA :: forall @f a. IsJsonDecoder f => a -> (Json -> f a) -> Json -> f a
toNullDefaultOrA def f =
  altAccumulateLazy (\j -> def <$ toJNull j) f

toNullNothingOrJust :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

toNullable :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Nullable a)
toNullable toA = withTypeHint "Nullable" $
  altAccumulateLazy (\j -> null <$ toJNull j) (\j -> notNull <$> toA j)

toBoolean :: forall @f. IsJsonDecoder f => Json -> f Boolean
toBoolean json =
  caseJson
    (\_ -> runFn2 onTypeMismatch "Boolean" "Null")
    pure
    (\_ -> runFn2 onTypeMismatch "Boolean" "Number")
    (\_ -> runFn2 onTypeMismatch "Boolean" "String")
    (\_ -> runFn2 onTypeMismatch "Boolean" "Array")
    (\_ -> runFn2 onTypeMismatch "Boolean" "Object")
    json

toNumber :: forall @f. IsJsonDecoder f => Json -> f Number
toNumber json =
  caseJson
    (\_ -> runFn2 onTypeMismatch "Number" "Null")
    (\_ -> runFn2 onTypeMismatch "Number" "Boolean")
    pure
    (\_ -> runFn2 onTypeMismatch "Number" "String")
    (\_ -> runFn2 onTypeMismatch "Number" "Array")
    (\_ -> runFn2 onTypeMismatch "Number" "Object")
    json

toInt :: forall @f. IsJsonDecoder f => Json -> f Int
toInt = withTypeHint "Int"
  $ toNumber >=> refine (\n -> note ("Could not convert Number to Int: " <> show n) $ Int.fromNumber n)

toString :: forall @f. IsJsonDecoder f => Json -> f String
toString json =
  caseJson
    (\_ -> runFn2 onTypeMismatch "String" "Null")
    (\_ -> runFn2 onTypeMismatch "String" "Boolean")
    (\_ -> runFn2 onTypeMismatch "String" "Number")
    pure
    (\_ -> runFn2 onTypeMismatch "String" "Array")
    (\_ -> runFn2 onTypeMismatch "String" "Object")
    json

toChar :: forall @f. IsJsonDecoder f => Json -> f Char
toChar = withTypeHint "Char"
  $ toString >=> refine (note "Could not get char at index 0 in String" <<< charAt 0)

toCodePoint
  :: forall @f
   . IsJsonDecoder f
  => Json
  -> f CodePoint
toCodePoint = withTypeHint "CodePoint" $
  toString
    >=> refine (\s -> note ("Could not get code point from String: " <> show s) $ codePointAt 0 s)

toNonEmptyString :: forall @f. IsJsonDecoder f => Json -> f NonEmptyString
toNonEmptyString = withTypeHint "NonEmptyString"
  $ toString >=> refine (note "Received empty string" <<< NonEmptyString.fromString)

toJArray :: forall @f. IsJsonDecoder f => Json -> f (Array Json)
toJArray json =
  caseJson
    (\_ -> runFn2 onTypeMismatch "Array" "Null")
    (\_ -> runFn2 onTypeMismatch "Array" "Boolean")
    (\_ -> runFn2 onTypeMismatch "Array" "Number")
    (\_ -> runFn2 onTypeMismatch "Array" "String")
    pure
    (\_ -> runFn2 onTypeMismatch "Array" "Object")
    json

underIndex' :: forall @f a. IsJsonDecoder f => Int -> (Maybe Json -> f a) -> Array Json -> f a
underIndex' idx f arr = f $ Array.index arr idx

underIndex :: forall @f a. IsJsonDecoder f => Int -> (Json -> f a) -> Array Json -> f a
underIndex idx f arr = arr # underIndex' idx case _ of
  Nothing -> onMissingIndex idx
  Just j -> withIndex idx f j

toJObject :: forall @f. IsJsonDecoder f => Json -> f (Object Json)
toJObject json =
  caseJson
    (\_ -> runFn2 onTypeMismatch "Object" "Null")
    (\_ -> runFn2 onTypeMismatch "Object" "Boolean")
    (\_ -> runFn2 onTypeMismatch "Object" "Number")
    (\_ -> runFn2 onTypeMismatch "Object" "String")
    (\_ -> runFn2 onTypeMismatch "Object" "Array")
    pure
    json

underKey' :: forall @f a. IsJsonDecoder f => String -> (Maybe Json -> f a) -> Object Json -> f a
underKey' key f obj = f $ Object.lookup key obj

underKey :: forall @f a. IsJsonDecoder f => String -> (Json -> f a) -> Object Json -> f a
underKey key f obj = obj # underKey' key case _ of
  Nothing -> onMissingField key
  Just j -> withKey key f j

toVoid :: forall @f. IsJsonDecoder f => Json -> f Void
toVoid _ = addTypeHint "Void" $ onUnrefinableValue "Decoding a value to Void is impossible"

toArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Array a)
toArray toElem = withTypeHint "Array" $
  toJArray >=> traverseWithIndex (flip withIndex toElem)

toArray2
  :: forall @f a b x
   . IsJsonDecoder f
  => (a -> b -> x)
  -> (Json -> f a)
  -> (Json -> f b)
  -> Json
  -> f x
toArray2 x a' b' = toJArray >=> case _ of
  [ a, b ] ->
    x
      <$> a' a
      <*> b' b
  arr -> onUnrefinableValue $ "Expected array of length 2 but array had length " <> show (Array.length arr)

toArray3
  :: forall @f a b c x
   . IsJsonDecoder f
  => (a -> b -> c -> x)
  -> (Json -> f a)
  -> (Json -> f b)
  -> (Json -> f c)
  -> Json
  -> f x
toArray3 x a' b' c' = toJArray >=> case _ of
  [ a, b, c ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
  arr -> onUnrefinableValue $ "Expected array of length 3 but array had length " <> show (Array.length arr)

toArray4
  :: forall @f a b c d x
   . IsJsonDecoder f
  => (a -> b -> c -> d -> x)
  -> (Json -> f a)
  -> (Json -> f b)
  -> (Json -> f c)
  -> (Json -> f d)
  -> Json
  -> f x
toArray4 x a' b' c' d' = toJArray >=> case _ of
  [ a, b, c, d ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
  arr -> onUnrefinableValue $ "Expected array of length 4 but array had length " <> show (Array.length arr)

toArray5
  :: forall @f a b c d e x
   . IsJsonDecoder f
  => (a -> b -> c -> d -> e -> x)
  -> (Json -> f a)
  -> (Json -> f b)
  -> (Json -> f c)
  -> (Json -> f d)
  -> (Json -> f e)
  -> Json
  -> f x
toArray5 x a' b' c' d' e' = toJArray >=> case _ of
  [ a, b, c, d, e ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
      <*> e' e
  arr -> onUnrefinableValue $ "Expected array of length 5 but array had length " <> show (Array.length arr)

toNonEmptyArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (NonEmptyArray a)
toNonEmptyArray toElem = withTypeHint "NonEmptyArray" $
  toArray toElem
    >=> refine (note "Received empty array" <<< NEA.fromArray)

toObject :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Object a)
toObject toElem = withTypeHint "Object" $ toJObject >=>
  traverseWithIndex (flip withKey toElem)

toIdentity :: forall @f a. Coercible (f a) (f (Identity a)) => IsJsonDecoder f => (Json -> f a) -> Json -> f (Identity a)
toIdentity f = withTypeHint "Identity" $ coerce f

toMaybeTagged :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Maybe a)
toMaybeTagged toElem = withTypeHint "Maybe" $ toJObject >=> \jo -> do
  tag <- jo # underKey "tag" toString
  case tag of
    "Just" -> addCtorHint "Just" $ (map Just <$> underKey "value" toElem) jo
    "Nothing" ->
      pure Nothing
    unknownTag ->
      onStructureError $ "Tag was not 'Just' or 'Nothing': " <> unknownTag

toEither
  :: forall @f a b
   . IsJsonDecoder f
  => (Json -> f a)
  -> (Json -> f b)
  -> Json
  -> f (Either a b)
toEither toLeft toRight = withTypeHint "Either" $ toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "Left" -> addCtorHint "Left" do
      Left <$> underKey "value" toLeft jo
    "Right" -> addCtorHint "Right" do
      Right <$> underKey "value" toRight jo
    unknownTag ->
      onStructureError $ "Tag was not 'Left' or 'Right': " <> unknownTag

toTuple
  :: forall @f a b
   . IsJsonDecoder f
  => (Json -> f a)
  -> (Json -> f b)
  -> Json
  -> f (Tuple a b)
toTuple toA toB = withTypeHint "Tuple" $
  toArray2
    Tuple
    (withSubtermHint 0 $ withIndex 0 toA)
    (withSubtermHint 1 $ withIndex 1 toB)

toThese
  :: forall @f a b
   . IsJsonDecoder f
  => (Json -> f a)
  -> (Json -> f b)
  -> Json
  -> f (These a b)
toThese toA toB = withTypeHint "These" $ toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "This" -> addCtorHint "This" do
      This <$> underKey "value" toA jo
    "That" -> addCtorHint "That" do
      That <$> underKey "value" toB jo
    "Both" -> addCtorHint "This" do
      Both
        <$> (addSubtermHint 0 $ underKey "this" toA jo)
        <*> (addSubtermHint 1 $ underKey "that" toB jo)
    unknownTag ->
      onStructureError $ "Tag was not 'This', 'That', or 'Both': " <> unknownTag

toNonEmpty
  :: forall @f g a
   . IsJsonDecoder f
  => (Json -> f a)
  -> (Json -> f (g a))
  -> (Json -> f (NonEmpty g a))
toNonEmpty toHead toTail = withTypeHint "NonEmpty" $ toJObject >=> \jo ->
  NonEmpty
    <$> (addSubtermHint 0 $ underKey "head" toHead jo)
    <*> (addSubtermHint 0 $ underKey "tail" toTail jo)

toList
  :: forall @f a
   . IsJsonDecoder f
  => (Json -> f a)
  -> Json
  -> f (List a)
toList toElem = withTypeHint "List" $
  toArray toElem
    >>> map List.fromFoldable

toNonEmptyList
  :: forall @f a
   . IsJsonDecoder f
  => (Json -> f a)
  -> Json
  -> f (NonEmptyList a)
toNonEmptyList toA = withTypeHint "NonEmptyList" $
  toList toA >=> refine (note "Received empty list" <<< NEL.fromList)

toMap
  :: forall @f k v
   . IsJsonDecoder f
  => Ord k
  => (Json -> f k)
  -> (Json -> f v)
  -> Json
  -> f (Map k v)
toMap toKey toValue = withTypeHint "Map"
  $ map (map Map.fromFoldable)
  $ toArray
  $ toJObject >=> \jo ->
      ( Tuple
          <$> (underKey "key" toKey jo)
          <*> (underKey "value" toValue jo)
      )

toSet
  :: forall @f a
   . IsJsonDecoder f
  => Ord a
  => (Json -> f a)
  -> Json
  -> f (Set a)
toSet toA = withTypeHint "Set" do
  toArray toA
    >>> map Set.fromFoldable

toNonEmptySet
  :: forall @f a
   . IsJsonDecoder f
  => Ord a
  => (Json -> f a)
  -> Json
  -> f (NonEmptySet a)
toNonEmptySet toA = withTypeHint "NonEmptySet" $
  toArray toA
    >=> refine (note "Received empty set" <<< NonEmptySet.fromSet <<< Set.fromFoldable)

coerce1 :: forall @f n a. Coercible (f n) (f a) => (a -> n) -> f a -> f n
coerce1 _ = coerce

-- | All labels must have a function of type: `ToRecordCodec a`
-- | See `required`, `requiredRename`, `option`, `optionRename`.
toRecord
  :: forall @f codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj f codecsRL { | codecs } { | values }
  => IsJsonDecoder f
  => { | codecs }
  -> Json
  -> f { | values }
toRecord codecs = toJObject >=>
  toRecordObj (Proxy :: Proxy codecsRL) codecs

-- | Variant of `toRecord` that coerces the `f { | r }` into a `f NewtypedR`
toRecordN
  :: forall @f n codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj f codecsRL { | codecs } { | values }
  => IsJsonDecoder f
  => Coercible (f n) (f { | values })
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> Json
  -> f n
toRecordN f codecs = coerce1 f <<< toRecord codecs

-- | Iterates through the underlying array.
-- | - key: uses the `str` in `Just str` or the record label if `Nothing`
-- | - f: the key used on the object and the result of looking up that key in the object
-- | - return: `Nothing` if the decoding failed; `Just` if it succeeded.
newtype ToRecordCodec f a = ToRecordCodec (Either a (NonEmptyArray (Tuple (Maybe String) (String -> Maybe Json -> f a))))

toStatic :: forall @f a. IsJsonDecoder f => a -> ToRecordCodec f a
toStatic a = ToRecordCodec $ Left a

toRequired :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f a
toRequired f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> onMissingField k
    Just j' -> withKey k f j'

toRequiredRename :: forall @f a. IsJsonDecoder f => String -> (Json -> f a) -> ToRecordCodec f a
toRequiredRename jsonLbl f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> onMissingField k
    Just j' -> withKey k f j'

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully tod.
toOption :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

toOptionRename :: forall @f a. IsJsonDecoder f => String -> (Json -> f a) -> ToRecordCodec f (Maybe a)
toOptionRename rename f = toOptionDefaultRename rename Nothing (map Just <$> f)

toOptionDefault :: forall @f a. IsJsonDecoder f => a -> (Json -> f a) -> ToRecordCodec f a
toOptionDefault a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> pure a
    Just j' -> withKey k f j'

toOptionDefaultRename :: forall @f a. IsJsonDecoder f => String -> a -> (Json -> f a) -> ToRecordCodec f a
toOptionDefaultRename jsonLbl a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> pure a
    Just j' -> withKey k f j'

toOptionArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f (Array a)
toOptionArray f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> toArray f j'

toOptionAssocArray :: forall @f a b. IsJsonDecoder f => (String -> f a) -> (Json -> f b) -> ToRecordCodec f (Array (Tuple a b))
toOptionAssocArray k' v' = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> (Object.toUnfoldable <$> toJObject j') >>= traverse (bitraverse k' v')

class ToRecordObj :: (Type -> Type) -> RowList Type -> Type -> Type -> Constraint
class ToRecordObj f codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> Object Json -> f values

instance (IsJsonDecoder f) => ToRecordObj f RL.Nil {} {} where
  toRecordObj _ _ _ = pure {}

instance
  ( ToRecordObj f codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Reflectable sym String
  , IsJsonDecoder f
  , Row.Cons sym (ToRecordCodec f a) cRest codecs
  , Row.Cons sym a vRest values
  , Row.Lacks sym vRest
  ) =>
  ToRecordObj f (RL.Cons sym (ToRecordCodec f a) codecTail) { | codecs } { | values } where
  toRecordObj _ codecs j = do
    rec <- toRecordObj (Proxy :: Proxy codecTail) codecsRest j
    a <- case keyDecoders of
      Left a' ->
        pure a'
      Right decs -> do
        j # withAttempts decs \(Tuple keyRename decoder) j' -> do
          let key = fromMaybe lbl keyRename
          decoder key (Object.lookup key j')
    pure $ Record.insert _lbl a rec
    where
    lbl = reflectType _lbl
    _lbl = (Proxy :: Proxy sym)
    (ToRecordCodec keyDecoders) = Record.get _lbl codecs
    codecsRest = unsafeCoerce codecs
else instance
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'ToRecordCodec f a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `toRequired*`/`toOption*` variants.")
          )
      )
  ) =>
  ToRecordObj f (RL.Cons sym a codecTail) { | codecs } { | values } where
  toRecordObj _ _ _ = unsafeCrashWith "Impossible"
