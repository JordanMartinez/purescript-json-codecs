-- @inline export Codec.Json.Unidirectional.Value.toVoid arity=1
-- @inline export Codec.Json.Unidirectional.Value.refine arity=1
-- @inline export Codec.Json.Unidirectional.Value.toJNull arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNullDefaultOrA arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNullNothingOrJust arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNullable arity=1
-- @inline export Codec.Json.Unidirectional.Value.toBoolean arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNumber arity=1
-- @inline export Codec.Json.Unidirectional.Value.toInt arity=1
-- @inline export Codec.Json.Unidirectional.Value.toString arity=1
-- @inline export Codec.Json.Unidirectional.Value.toChar arity=1
-- @inline export Codec.Json.Unidirectional.Value.toCodePoint arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNonEmptyString arity=1
-- @inline export Codec.Json.Unidirectional.Value.toJArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.underIndex' arity=1
-- @inline export Codec.Json.Unidirectional.Value.underIndex arity=1
-- @inline export Codec.Json.Unidirectional.Value.toArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.toArray2 arity=1
-- @inline export Codec.Json.Unidirectional.Value.toArray3 arity=1
-- @inline export Codec.Json.Unidirectional.Value.toArray4 arity=1
-- @inline export Codec.Json.Unidirectional.Value.toArray5 arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNonEmptyArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.toJObject arity=1
-- @inline export Codec.Json.Unidirectional.Value.underKey arity=1
-- @inline export Codec.Json.Unidirectional.Value.underKey' arity=1
-- @inline export Codec.Json.Unidirectional.Value.toObject arity=1
-- @inline export Codec.Json.Unidirectional.Value.toObjSingleton arity=1
-- @inline export Codec.Json.Unidirectional.Value.toIdentity arity=1
-- @inline export Codec.Json.Unidirectional.Value.toMaybeTagged arity=1
-- @inline export Codec.Json.Unidirectional.Value.toEither arity=1
-- @inline export Codec.Json.Unidirectional.Value.toTuple arity=1
-- @inline export Codec.Json.Unidirectional.Value.toThese arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNonEmpty arity=1
-- @inline export Codec.Json.Unidirectional.Value.toList arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNonEmptyList arity=1
-- @inline export Codec.Json.Unidirectional.Value.toMap arity=1
-- @inline export Codec.Json.Unidirectional.Value.toSet arity=1
-- @inline export Codec.Json.Unidirectional.Value.toNonEmptySet arity=1
-- @inline export Codec.Json.Unidirectional.Value.toEither arity=1
-- @inline export Codec.Json.Unidirectional.Value.fromRecord arity=2
-- @inline export Codec.Json.Unidirectional.Value.toRecord arity=3
-- @inline export Codec.Json.Unidirectional.Value.fromRecordN arity=3
-- @inline export Codec.Json.Unidirectional.Value.toRecordN arity=4
-- @inline export Codec.Json.Unidirectional.Value.toStatic arity=1
-- @inline export Codec.Json.Unidirectional.Value.toRequired arity=1
-- @inline export Codec.Json.Unidirectional.Value.toRequiredRename arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOption arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionRename arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionDefault arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionDefaultRename arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionAssocArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.toRecordObjNil(..).toRecordObj arity=1
-- @inline export Codec.Json.Unidirectional.Value.toRecordObjCons(..).toRecordObj arity=7
-- @inline export Codec.Json.Unidirectional.Value.fromRecordObjCons(..).fromRecordObj arity=5
module Codec.Json.Unidirectional.Value
  ( refine
  , coerce1
  , fromVoid
  , toVoid
  , fromUnit
  , fromJNull
  , toJNull
  , toNullDefaultOrA
  , fromNullNothingOrJust
  , toNullNothingOrJust
  , fromNullable
  , toNullable
  , fromBoolean
  , toBoolean
  , fromNumber
  , toNumber
  , fromInt
  , toInt
  , fromString
  , toString
  , fromChar
  , toChar
  , fromCodePoint
  , toCodePoint
  , fromNonEmptyString
  , toNonEmptyString
  , fromJArray
  , toJArray
  , underIndex
  , underIndex'
  , fromArray
  , toArray
  , fromArray2
  , toArray2
  , fromArray3
  , toArray3
  , fromArray4
  , toArray4
  , fromArray5
  , toArray5
  , fromNonEmptyArray
  , toNonEmptyArray
  , fromJObject
  , toJObject
  , underKey
  , underKey'
  , fromObject
  , toObject
  , fromObjSingleton
  , toObjSingleton
  , fromPropArray
  , fromIdentity
  , toIdentity
  , fromMaybeTagged
  , toMaybeTagged
  , fromEither
  , toEither
  , fromTuple
  , toTuple
  , fromThese
  , toThese
  , fromNonEmpty
  , toNonEmpty
  , fromList
  , toList
  , fromNonEmptyList
  , toNonEmptyList
  , fromMap
  , toMap
  , fromSet
  , toSet
  , fromNonEmptySet
  , toNonEmptySet
  , fromRecord
  , toRecord
  , fromRecordN
  , toRecordN
  , FromRecordCodec(..)
  , ToRecordCodec(..)
  , toStatic
  , fromRequired
  , toRequired
  , fromRequiredRename
  , toRequiredRename
  , fromOption
  , toOption
  , fromOptionRename
  , toOptionRename
  , toOptionDefault
  , toOptionDefaultRename
  , fromOptionArray
  , toOptionArray
  , fromOptionAssocArray
  , toOptionAssocArray
  , class ToRecordObj
  , toRecordObj
  , class FromRecordObj
  , fromRecordObj
  ) where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder, addCtorHint, addSubtermHint, addTypeHint, altAccumulateLazy, onMissingField, onMissingIndex, onStructureError, onTypeMismatch, onUnrefinableValue, withAttempts, withIndex, withKey, withSubtermHint, withTypeHint)
import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (runFn2)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String (CodePoint, codePointAt)
import Data.String as SCP
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol)
import Data.These (These(..), these)
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

fromVoid :: Void -> Json
fromVoid = absurd

toVoid :: forall @f. IsJsonDecoder f => Json -> f Void
toVoid _ = addTypeHint "Void" $ onUnrefinableValue "Decoding a value to Void is impossible"

coerce1 :: forall @f n a. Coercible (f n) (f a) => (a -> n) -> f a -> f n
coerce1 _ = coerce

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

fromJNull :: Json
fromJNull = Json.jsonNull

fromUnit :: Unit -> Json
fromUnit = const fromJNull

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

fromNullNothingOrJust :: forall a. (a -> Json) -> Maybe a -> Json
fromNullNothingOrJust f = maybe Json.jsonNull f

toNullNothingOrJust :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

fromNullable :: forall a. (a -> Json) -> Nullable a -> Json
fromNullable fromA = toMaybe >>> fromNullNothingOrJust fromA

toNullable :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Nullable a)
toNullable toA = withTypeHint "Nullable" $
  altAccumulateLazy (\j -> null <$ toJNull j) (\j -> notNull <$> toA j)

fromBoolean :: Boolean -> Json
fromBoolean = Json.fromBoolean

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

fromNumber :: Number -> Json
fromNumber = Json.fromNumber

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

fromInt :: Int -> Json
fromInt = Int.toNumber >>> fromNumber

fromString :: String -> Json
fromString = Json.fromString

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

fromChar :: Char -> Json
fromChar = SCU.singleton >>> fromString

toChar :: forall @f. IsJsonDecoder f => Json -> f Char
toChar = withTypeHint "Char"
  $ toString >=> refine (note "Could not get char at index 0 in String" <<< charAt 0)

fromCodePoint :: CodePoint -> Json
fromCodePoint = SCP.singleton >>> fromString

toCodePoint
  :: forall @f
   . IsJsonDecoder f
  => Json
  -> f CodePoint
toCodePoint = withTypeHint "CodePoint" $
  toString
    >=> refine (\s -> note ("Could not get code point from String: " <> show s) $ codePointAt 0 s)

fromNonEmptyString :: NonEmptyString -> Json
fromNonEmptyString (NonEmptyString s) = fromString s

toNonEmptyString :: forall @f. IsJsonDecoder f => Json -> f NonEmptyString
toNonEmptyString = withTypeHint "NonEmptyString"
  $ toString >=> refine (note "Received empty string" <<< NonEmptyString.fromString)

fromJArray :: Array Json -> Json
fromJArray = Json.fromArray

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

fromArray :: forall a. (a -> Json) -> Array a -> Json
fromArray fromA = map fromA >>> fromJArray

toArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Array a)
toArray toElem = withTypeHint "Array" $
  toJArray >=> traverseWithIndex (flip withIndex toElem)

fromArray2 :: Json -> Json -> Json
fromArray2 a b = fromJArray [ a, b ]

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

fromArray3 :: Json -> Json -> Json -> Json
fromArray3 a b c = fromJArray [ a, b, c ]

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

fromArray4 :: Json -> Json -> Json -> Json -> Json
fromArray4 a b c d = fromJArray [ a, b, c, d ]

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

fromArray5 :: Json -> Json -> Json -> Json -> Json -> Json
fromArray5 a b c d e = fromJArray [ a, b, c, d, e ]

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

fromNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
fromNonEmptyArray fromA = NEA.toArray >>> fromArray fromA

toNonEmptyArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (NonEmptyArray a)
toNonEmptyArray toElem = withTypeHint "NonEmptyArray" $
  toArray toElem
    >=> refine (note "Received empty array" <<< NEA.fromArray)

fromJObject :: Object Json -> Json
fromJObject = Json.fromObject

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

fromObject :: forall a. (a -> Json) -> Object a -> Json
fromObject fromA = map fromA >>> fromJObject

toObject :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Object a)
toObject toElem = withTypeHint "Object" $ toJObject >=>
  traverseWithIndex (flip withKey toElem)

fromObjSingleton :: String -> Json -> Json
fromObjSingleton k v = fromJObject $ Object.singleton k v

toObjSingleton :: forall f a. IsJsonDecoder f => String -> (Maybe Json -> f a) -> Json -> f a
toObjSingleton k f = toJObject >=> (\j -> underKey' k f j)

fromPropArray :: Array (Tuple String Json) -> Json
fromPropArray = fromJObject <<< Array.foldl (\acc (Tuple k v) -> Object.insert k v acc) Object.empty

fromIdentity :: forall a. (a -> Json) -> Identity a -> Json
fromIdentity fromA (Identity a) = fromA a

toIdentity :: forall @f a. Coercible (f a) (f (Identity a)) => IsJsonDecoder f => (Json -> f a) -> Json -> f (Identity a)
toIdentity f = withTypeHint "Identity" $ coerce f

fromMaybeTagged :: forall a. (a -> Json) -> Maybe a -> Json
fromMaybeTagged fromA =
  maybe (Object.singleton "tag" $ fromString "Nothing") (tagged "Just" <<< fromA)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toMaybeTagged :: forall @f a. IsJsonDecoder f => (Json -> f a) -> Json -> f (Maybe a)
toMaybeTagged toElem = withTypeHint "Maybe" $ toJObject >=> \jo -> do
  tag <- jo # underKey "tag" toString
  case tag of
    "Just" -> addCtorHint "Just" $ (map Just <$> underKey "value" toElem) jo
    "Nothing" ->
      pure Nothing
    unknownTag ->
      onStructureError $ "Tag was not 'Just' or 'Nothing': " <> unknownTag

fromEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEither fromA fromB =
  either (tagged "Left" <<< fromA) (tagged "Right" <<< fromB)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

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

fromTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
fromTuple fromA fromB (Tuple a b) =
  fromJArray
    [ fromA a
    , fromB b
    ]

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

fromThese :: forall a b. (a -> Json) -> (b -> Json) -> These a b -> Json
fromThese fromA fromB =
  these
    (tagged "This" <<< fromA)
    (tagged "That" <<< fromB)
    ( \a b -> tagged "Both" $ fromJObject $ Object.fromFoldable
        [ Tuple "this" $ fromA a
        , Tuple "that" $ fromB b
        ]
    )
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

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

fromNonEmpty :: forall f a. (a -> Json) -> (f a -> Json) -> NonEmpty f a -> Json
fromNonEmpty fromHead fromTail (NonEmpty a fa) =
  fromJObject
    $ Object.fromFoldable
        [ Tuple "head" $ fromHead a
        , Tuple "tail" $ fromTail fa
        ]

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

fromList :: forall a. (a -> Json) -> List a -> Json
fromList fromA = List.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toList
  :: forall @f a
   . IsJsonDecoder f
  => (Json -> f a)
  -> Json
  -> f (List a)
toList toElem = withTypeHint "List" $
  toArray toElem
    >>> map List.fromFoldable

fromNonEmptyList :: forall a. (a -> Json) -> (NonEmptyList a -> Json)
fromNonEmptyList fromA = NEL.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toNonEmptyList
  :: forall @f a
   . IsJsonDecoder f
  => (Json -> f a)
  -> Json
  -> f (NonEmptyList a)
toNonEmptyList toA = withTypeHint "NonEmptyList" $
  toList toA >=> refine (note "Received empty list" <<< NEL.fromList)

fromMap :: forall k v. (k -> Json) -> (v -> Json) -> Map k v -> Json
fromMap fromKey fromValue =
  foldlWithIndex
    ( \k acc v -> Array.snoc acc
        $ fromJObject
        $ Object.fromFoldable
            [ Tuple "key" $ fromKey k
            , Tuple "value" $ fromValue v
            ]
    )
    []
    >>> fromJArray

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

fromSet :: forall a. (a -> Json) -> Set a -> Json
fromSet fromA = foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

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

fromNonEmptySet :: forall a. (a -> Json) -> NonEmptySet a -> Json
fromNonEmptySet fromA = NonEmptySet.toSet >>> fromSet fromA

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

-- | All labels must have a function of type: `FromRecordCodec a`
fromRecord
  :: forall codecs values codecsRL
   . RowToList codecs codecsRL
  => FromRecordObj codecsRL { | codecs } { | values }
  => { | codecs }
  -> { | values }
  -> Json
fromRecord codecs values = Json.fromObject
  $ fromRecordObj (Proxy :: Proxy codecsRL) codecs values

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

fromRecordN
  :: forall n codecs values codecsRL
   . RowToList codecs codecsRL
  => FromRecordObj codecsRL { | codecs } { | values }
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> n
  -> Json
fromRecordN _ codecs = unwrap >>> fromRecord codecs

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

newtype FromRecordCodec a = FromRecordCodec (Tuple (Maybe String) (String -> a -> Maybe Json))

toStatic :: forall @f a. IsJsonDecoder f => a -> ToRecordCodec f a
toStatic a = ToRecordCodec $ Left a

fromRequired :: forall a. (a -> Json) -> FromRecordCodec a
fromRequired f = FromRecordCodec $ Tuple Nothing \_ -> Just <<< f

toRequired :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f a
toRequired f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> onMissingField k
    Just j' -> withKey k f j'

fromRequiredRename :: forall a. String -> (a -> Json) -> FromRecordCodec a
fromRequiredRename str f = FromRecordCodec $ Tuple (Just str) \_ -> Just <<< f

toRequiredRename :: forall @f a. IsJsonDecoder f => String -> (Json -> f a) -> ToRecordCodec f a
toRequiredRename jsonLbl f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> onMissingField k
    Just j' -> withKey k f j'

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> Json) -> FromRecordCodec (Maybe a)
fromOption f = FromRecordCodec $ Tuple Nothing \_ -> map f

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully tod.
toOption :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

fromOptionRename :: forall a. String -> (a -> Json) -> FromRecordCodec (Maybe a)
fromOptionRename str f = FromRecordCodec $ Tuple (Just str) \_ -> map f

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

fromOptionArray :: forall a. (a -> Json) -> FromRecordCodec (Array a)
fromOptionArray f = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ fromArray f arr

toOptionArray :: forall @f a. IsJsonDecoder f => (Json -> f a) -> ToRecordCodec f (Array a)
toOptionArray f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> toArray f j'

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> Json) -> FromRecordCodec (Array (Tuple a b))
fromOptionAssocArray k' v' = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

toOptionAssocArray :: forall @f a b. IsJsonDecoder f => (String -> f a) -> (Json -> f b) -> ToRecordCodec f (Array (Tuple a b))
toOptionAssocArray k' v' = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> (Object.toUnfoldable <$> toJObject j') >>= traverse (bitraverse k' v')

class ToRecordObj :: (Type -> Type) -> RowList Type -> Type -> Type -> Constraint
class ToRecordObj f codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> Object Json -> f values

instance toRecordObjNil :: (IsJsonDecoder f) => ToRecordObj f RL.Nil {} {} where
  toRecordObj _ _ _ = pure {}

instance toRecordObjCons ::
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

class FromRecordObj :: RowList Type -> Type -> Type -> Constraint
class FromRecordObj codecsRL codecs values | codecsRL -> codecs values where
  fromRecordObj :: Proxy codecsRL -> codecs -> values -> Object Json

instance fromRecordObjNil :: FromRecordObj RL.Nil {} {} where
  fromRecordObj _ _ _ = Object.empty

instance fromRecordObjCons ::
  ( FromRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Reflectable sym String
  , Row.Cons sym (FromRecordCodec a) cRest codecs
  , Row.Cons sym a vRest values
  ) =>
  FromRecordObj (RL.Cons sym (FromRecordCodec a) codecTail) { | codecs } { | values } where
  fromRecordObj _ codecs values = do
    let obj = fromRecordObj (Proxy :: Proxy codecTail) cRest vRest
    let key = fromMaybe lbl keyRename
    case encoder key a' of
      Nothing -> obj
      Just a'' -> Object.insert key a'' obj
    where
    lbl = reflectType _lbl
    _lbl = (Proxy :: Proxy sym)
    (FromRecordCodec (Tuple keyRename encoder)) = Record.get _lbl codecs
    a' = Record.get _lbl values
    cRest = unsafeCoerce codecs
    vRest = unsafeCoerce values
else instance
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'FromRecordCodec a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `fromRequired*`/`fromOption*` variants.")
          )
      )
  ) =>
  FromRecordObj (RL.Cons sym a codecTail) { | codecs } { | values } where
  fromRecordObj _ _ _ = unsafeCrashWith "Impossible"
