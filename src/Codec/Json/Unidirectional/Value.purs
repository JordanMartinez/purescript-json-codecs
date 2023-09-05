-- @inline export Codec.Json.Unidirectional.Value.withIndex arity=1
-- @inline export Codec.Json.Unidirectional.Value.wthKey arity=1
-- @inline export Codec.Json.Unidirectional.Value.withAttempts arity=2
-- @inline export Codec.Json.Unidirectional.Value.altAccumulateLazy arity=2
-- @inline export Codec.Json.Unidirectional.Value.underIndex' arity=1
-- @inline export Codec.Json.Unidirectional.Value.underIndex arity=1
-- @inline export Codec.Json.Unidirectional.Value.underKey arity=1
-- @inline export Codec.Json.Unidirectional.Value.underKey' arity=1
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
-- @inline export Codec.Json.Unidirectional.Value.toRecord arity=2
-- @inline export Codec.Json.Unidirectional.Value.fromRecordN arity=3
-- @inline export Codec.Json.Unidirectional.Value.toRecordN arity=3
-- @inline export Codec.Json.Unidirectional.Value.toStatic arity=1
-- @inline export Codec.Json.Unidirectional.Value.fromRequired arity=1
-- @inline export Codec.Json.Unidirectional.Value.toRequired arity=1
-- @inline export Codec.Json.Unidirectional.Value.fromRequiredRename arity=2
-- @inline export Codec.Json.Unidirectional.Value.toRequiredRename arity=2
-- @inline export Codec.Json.Unidirectional.Value.fromOption arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOption arity=1
-- @inline export Codec.Json.Unidirectional.Value.fromOptionRename arity=2
-- @inline export Codec.Json.Unidirectional.Value.toOptionRename arity=2
-- @inline export Codec.Json.Unidirectional.Value.toOptionDefault arity=2
-- @inline export Codec.Json.Unidirectional.Value.toOptionDefaultRename arity=3
-- @inline export Codec.Json.Unidirectional.Value.fromOptionArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.toOptionArray arity=1
-- @inline export Codec.Json.Unidirectional.Value.fromOptionAssocArray arity=2
-- @inline export Codec.Json.Unidirectional.Value.toOptionAssocArray arity=2
-- @inline export Codec.Json.Unidirectional.Value.toRecordObjNil(..).toRecordObj always
-- @inline export Codec.Json.Unidirectional.Value.toRecordObjCons(..).toRecordObj arity=5
-- @inline export Codec.Json.Unidirectional.Value.toRecordObjFailure(..).fromRecordObj always
-- @inline export Codec.Json.Unidirectional.Value.fromRecordObjNil(..).fromRecordObj always
-- @inline export Codec.Json.Unidirectional.Value.fromRecordObjCons(..).fromRecordObj arity=5
-- @inline export Codec.Json.Unidirectional.Value.fromRecordObjFailure(..).fromRecordObj always
module Codec.Json.Unidirectional.Value
  ( coerce1
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

import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..))
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
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

withIndex :: forall a. Int -> Either (List String) a -> Either (List String) a
withIndex idx = lmap (Cons $ "[" <> show idx <> "]")

withKey :: forall a. String -> Either (List String) a -> Either (List String) a
withKey key = lmap (Cons $ "." <> show key)

withAttempts :: forall a j b. NonEmptyArray a -> (a -> j -> Either (List String) b) -> j -> Either (List String) b
withAttempts decoders fn j = go 0
  where
  lastIdx = NEA.length decoders - 1
  go idx = case fn (unsafePartial $ NEA.unsafeIndex decoders idx) j of
    x@(Right _) -> x
    x@(Left _)
      | idx == lastIdx -> x
      | otherwise -> go (idx + 1)

altAccumulateLazy :: forall a. (Json -> Either (List String) a) -> (Json -> Either (List String) a) -> Json -> Either (List String) a
altAccumulateLazy f g j = case f j of
  x@(Right _) -> x
  (Left e1) -> case g j of
    x@(Right _) -> x
    Left e2 -> Left $ e1 <> e2

fromVoid :: Void -> Json
fromVoid = absurd

toVoid :: Json -> Either (List String) Void
toVoid _ = Left $ pure "Decoding a value to Void is impossible"

coerce1 :: forall n a. Coercible n a => (a -> n) -> Either (List String) a -> Either (List String) n
coerce1 _ = coerce

fromJNull :: Json
fromJNull = Json.jsonNull

fromUnit :: Unit -> Json
fromUnit = const fromJNull

toJNull :: Json -> Either (List String) Unit
toJNull json =
  caseJson
    pure
    (\_ -> Left $ pure "Expected a value of type Null but got Boolean")
    (\_ -> Left $ pure "Expected a value of type Null but got Number")
    (\_ -> Left $ pure "Expected a value of type Null but got String")
    (\_ -> Left $ pure "Expected a value of type Null but got Array")
    (\_ -> Left $ pure "Expected a value of type Null but got Object")
    json

toNullDefaultOrA :: forall a. a -> (Json -> Either (List String) a) -> Json -> Either (List String) a
toNullDefaultOrA def f = altAccumulateLazy (\j -> def <$ toJNull j) f

fromNullNothingOrJust :: forall a. (a -> Json) -> Maybe a -> Json
fromNullNothingOrJust f = maybe Json.jsonNull f

toNullNothingOrJust :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

fromNullable :: forall a. (a -> Json) -> Nullable a -> Json
fromNullable fromA = toMaybe >>> fromNullNothingOrJust fromA

toNullable :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Nullable a)
toNullable toA = altAccumulateLazy (\j -> null <$ toJNull j) (\j -> notNull <$> toA j)

fromBoolean :: Boolean -> Json
fromBoolean = Json.fromBoolean

toBoolean :: Json -> Either (List String) Boolean
toBoolean json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Boolean but got Null")
    pure
    (\_ -> Left $ pure "Expected a value of type Boolean but got Number")
    (\_ -> Left $ pure "Expected a value of type Boolean but got String")
    (\_ -> Left $ pure "Expected a value of type Boolean but got Array")
    (\_ -> Left $ pure "Expected a value of type Boolean but got Object")
    json

fromNumber :: Number -> Json
fromNumber = Json.fromNumber

toNumber :: Json -> Either (List String) Number
toNumber json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Number but got Null")
    (\_ -> Left $ pure "Expected a value of type Number but got Boolean")
    pure
    (\_ -> Left $ pure "Expected a value of type Number but got String")
    (\_ -> Left $ pure "Expected a value of type Number but got Array")
    (\_ -> Left $ pure "Expected a value of type Number but got Object")
    json

fromInt :: Int -> Json
fromInt = Int.toNumber >>> fromNumber

fromString :: String -> Json
fromString = Json.fromString

toInt :: Json -> Either (List String) Int
toInt = toNumber >=> (\n -> note (pure $ "could not convert Number to Int: " <> show n) $ Int.fromNumber n)

toString :: Json -> Either (List String) String
toString json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type String but got Null")
    (\_ -> Left $ pure "Expected a value of type String but got Boolean")
    (\_ -> Left $ pure "Expected a value of type String but got Number")
    pure
    (\_ -> Left $ pure "Expected a value of type String but got Array")
    (\_ -> Left $ pure "Expected a value of type String but got Object")
    json

fromChar :: Char -> Json
fromChar = SCU.singleton >>> fromString

toChar :: Json -> Either (List String) Char
toChar = toString >=> (note (pure "Could not get char at index 0 in String") <<< charAt 0)

fromCodePoint :: CodePoint -> Json
fromCodePoint = SCP.singleton >>> fromString

toCodePoint :: Json -> Either (List String) CodePoint
toCodePoint = toString >=> (\s -> note (pure $ "Could not get code point from String: " <> show s) $ codePointAt 0 s)

fromNonEmptyString :: NonEmptyString -> Json
fromNonEmptyString (NonEmptyString s) = fromString s

toNonEmptyString :: Json -> Either (List String) NonEmptyString
toNonEmptyString = toString >=> (note (pure $ "Received empty string") <<< NonEmptyString.fromString)

fromJArray :: Array Json -> Json
fromJArray = Json.fromArray

toJArray :: Json -> Either (List String) (Array Json)
toJArray json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Array but got Null")
    (\_ -> Left $ pure "Expected a value of type Array but got Boolean")
    (\_ -> Left $ pure "Expected a value of type Array but got Number")
    (\_ -> Left $ pure "Expected a value of type Array but got String")
    pure
    (\_ -> Left $ pure "Expected a value of type Array but got Object")
    json

underIndex' :: forall a. Int -> (Maybe Json -> Either (List String) a) -> Array Json -> Either (List String) a
underIndex' idx f arr = do
  let res = Array.index arr idx
  case res of
    Nothing -> f res
    Just _ -> withIndex idx $ f res

underIndex :: forall a. Int -> (Json -> Either (List String) a) -> Array Json -> Either (List String) a
underIndex idx f arr = arr # underIndex' idx case _ of
  Nothing -> Left $ pure $ "Missing index: " <> show idx
  Just j -> withIndex idx $ f j

fromArray :: forall a. (a -> Json) -> Array a -> Json
fromArray fromA = map fromA >>> fromJArray

toArray :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> withIndex i $ toElem j)

fromArray2 :: Json -> Json -> Json
fromArray2 a b = fromJArray [ a, b ]

toArray2
  :: forall a b x
   . (a -> b -> x)
  -> (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> Json
  -> Either (List String) x
toArray2 x a' b' = toJArray >=> case _ of
  [ a, b ] ->
    x
      <$> a' a
      <*> b' b
  arr -> Left $ pure $ "Expected array of length 2 but array had length " <> show (Array.length arr)

fromArray3 :: Json -> Json -> Json -> Json
fromArray3 a b c = fromJArray [ a, b, c ]

toArray3
  :: forall a b c x
   . (a -> b -> c -> x)
  -> (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> (Json -> Either (List String) c)
  -> Json
  -> Either (List String) x
toArray3 x a' b' c' = toJArray >=> case _ of
  [ a, b, c ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
  arr -> Left $ pure $ "Expected array of length 3 but array had length " <> show (Array.length arr)

fromArray4 :: Json -> Json -> Json -> Json -> Json
fromArray4 a b c d = fromJArray [ a, b, c, d ]

toArray4
  :: forall a b c d x
   . (a -> b -> c -> d -> x)
  -> (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> (Json -> Either (List String) c)
  -> (Json -> Either (List String) d)
  -> Json
  -> Either (List String) x
toArray4 x a' b' c' d' = toJArray >=> case _ of
  [ a, b, c, d ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
  arr -> Left $ pure $ "Expected array of length 4 but array had length " <> show (Array.length arr)

fromArray5 :: Json -> Json -> Json -> Json -> Json -> Json
fromArray5 a b c d e = fromJArray [ a, b, c, d, e ]

toArray5
  :: forall a b c d e x
   . (a -> b -> c -> d -> e -> x)
  -> (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> (Json -> Either (List String) c)
  -> (Json -> Either (List String) d)
  -> (Json -> Either (List String) e)
  -> Json
  -> Either (List String) x
toArray5 x a' b' c' d' e' = toJArray >=> case _ of
  [ a, b, c, d, e ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
      <*> e' e
  arr -> Left $ pure $ "Expected array of length 5 but array had length " <> show (Array.length arr)

fromNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
fromNonEmptyArray fromA = NEA.toArray >>> fromArray fromA

toNonEmptyArray :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (NonEmptyArray a)
toNonEmptyArray toElem = toArray toElem >=> (note (pure "Received empty array") <<< NEA.fromArray)

fromJObject :: Object Json -> Json
fromJObject = Json.fromObject

toJObject :: Json -> Either (List String) (Object Json)
toJObject json =
  caseJson
    (\_ -> Left $ pure "Expected a value of type Object but got Null")
    (\_ -> Left $ pure "Expected a value of type Object but got Boolean")
    (\_ -> Left $ pure "Expected a value of type Object but got Number")
    (\_ -> Left $ pure "Expected a value of type Object but got String")
    (\_ -> Left $ pure "Expected a value of type Object but got Array")
    pure
    json

underKey' :: forall a. String -> (Maybe Json -> Either (List String) a) -> Object Json -> Either (List String) a
underKey' key f obj = do
  let res = Object.lookup key obj
  case res of
    Nothing -> f res
    Just _ -> withKey key $ f res

underKey :: forall a. String -> (Json -> Either (List String) a) -> Object Json -> Either (List String) a
underKey key f obj = obj # underKey' key case _ of
  Nothing -> Left $ pure $ "Missing key, " <> show key
  Just j -> withKey key $ f j

fromObject :: forall a. (a -> Json) -> Object a -> Json
fromObject fromA = map fromA >>> fromJObject

toObject :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Object a)
toObject toElem = toJObject >=> traverseWithIndex (\k j -> withKey k $ toElem j)

fromObjSingleton :: String -> Json -> Json
fromObjSingleton k v = fromJObject $ Object.singleton k v

toObjSingleton :: forall a. String -> (Maybe Json -> Either (List String) a) -> Json -> Either (List String) a
toObjSingleton k f = toJObject >=> (\j -> underKey' k f j)

fromPropArray :: Array (Tuple String Json) -> Json
fromPropArray = fromJObject <<< Array.foldl (\acc (Tuple k v) -> Object.insert k v acc) Object.empty

fromIdentity :: forall a. (a -> Json) -> Identity a -> Json
fromIdentity fromA (Identity a) = fromA a

toIdentity :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Identity a)
toIdentity f = coerce f

fromMaybeTagged :: forall a. (a -> Json) -> Maybe a -> Json
fromMaybeTagged fromA =
  maybe (Object.singleton "tag" $ fromString "Nothing") (tagged "Just" <<< fromA)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toMaybeTagged :: forall a. (Json -> Either (List String) a) -> Json -> Either (List String) (Maybe a)
toMaybeTagged toElem = toJObject >=> \jo -> do
  tag <- jo # underKey "tag" toString
  case tag of
    "Just" -> (map Just <$> underKey "value" toElem) jo
    "Nothing" ->
      pure Nothing
    unknownTag ->
      Left $ pure $ "Tag was not 'Just' or 'Nothing': " <> unknownTag

fromEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEither fromA fromB =
  either (tagged "Left" <<< fromA) (tagged "Right" <<< fromB)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toEither
  :: forall a b
   . (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> Json
  -> Either (List String) (Either a b)
toEither toLeft toRight = toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "Left" -> Left <$> underKey "value" toLeft jo
    "Right" -> Right <$> underKey "value" toRight jo
    unknownTag ->
      Left $ pure $ "Tag was not 'Left' or 'Right': " <> unknownTag

fromTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
fromTuple fromA fromB (Tuple a b) =
  fromJArray
    [ fromA a
    , fromB b
    ]

toTuple
  :: forall a b
   . (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> Json
  -> Either (List String) (Tuple a b)
toTuple toA toB =
  toArray2
    Tuple
    toA
    toB

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
  :: forall a b
   . (Json -> Either (List String) a)
  -> (Json -> Either (List String) b)
  -> Json
  -> Either (List String) (These a b)
toThese toA toB = toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "This" -> This <$> underKey "value" toA jo
    "That" -> That <$> underKey "value" toB jo
    "Both" ->
      Both
        <$> (underKey "this" toA jo)
        <*> (underKey "that" toB jo)
    unknownTag ->
      Left $ pure $ "Tag was not 'This', 'That', or 'Both': " <> unknownTag

fromNonEmpty :: forall f a. (a -> Json) -> (f a -> Json) -> NonEmpty f a -> Json
fromNonEmpty fromHead fromTail (NonEmpty a fa) =
  fromJObject
    $ Object.fromFoldable
        [ Tuple "head" $ fromHead a
        , Tuple "tail" $ fromTail fa
        ]

toNonEmpty
  :: forall g a
   . (Json -> Either (List String) a)
  -> (Json -> Either (List String) (g a))
  -> (Json -> Either (List String) (NonEmpty g a))
toNonEmpty toHead toTail = toJObject >=> \jo ->
  NonEmpty
    <$> (underKey "head" toHead jo)
    <*> (underKey "tail" toTail jo)

fromList :: forall a. (a -> Json) -> List a -> Json
fromList fromA = List.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toList
  :: forall a
   . (Json -> Either (List String) a)
  -> Json
  -> Either (List String) (List a)
toList toElem = toArray toElem >>> map List.fromFoldable

fromNonEmptyList :: forall a. (a -> Json) -> (NonEmptyList a -> Json)
fromNonEmptyList fromA = NEL.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toNonEmptyList
  :: forall a
   . (Json -> Either (List String) a)
  -> Json
  -> Either (List String) (NonEmptyList a)
toNonEmptyList toA = toList toA >=> (note (pure $ "Received empty list") <<< NEL.fromList)

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
  :: forall k v
   . Ord k
  => (Json -> Either (List String) k)
  -> (Json -> Either (List String) v)
  -> Json
  -> Either (List String) (Map k v)
toMap toKey toValue =
  toArray
    ( toJObject >=> \jo ->
        ( Tuple
            <$> (underKey "key" toKey jo)
            <*> (underKey "value" toValue jo)
        )
    )
    >>> map Map.fromFoldable

fromSet :: forall a. (a -> Json) -> Set a -> Json
fromSet fromA = foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toSet
  :: forall a
   . Ord a
  => (Json -> Either (List String) a)
  -> Json
  -> Either (List String) (Set a)
toSet toA = toArray toA >>> map Set.fromFoldable

fromNonEmptySet :: forall a. (a -> Json) -> NonEmptySet a -> Json
fromNonEmptySet fromA = NonEmptySet.toSet >>> fromSet fromA

toNonEmptySet
  :: forall a
   . Ord a
  => (Json -> Either (List String) a)
  -> Json
  -> Either (List String) (NonEmptySet a)
toNonEmptySet toA = toArray toA >=> (note (pure $ "Received empty set") <<< NonEmptySet.fromSet <<< Set.fromFoldable)

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
  :: forall codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj codecsRL { | codecs } { | values }
  => { | codecs }
  -> Json
  -> Either (List String) { | values }
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
  :: forall n codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj codecsRL { | codecs } { | values }
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> Json
  -> Either (List String) n
toRecordN f codecs = coerce1 f <<< toRecord codecs

-- | Iterates through the underlying array.
-- | - key: uses the `str` in `Just str` or the record label if `Nothing`
-- | - f: the key used on the object and the result of looking up that key in the object
-- | - return: `Nothing` if the decoding failed; `Just` if it succeeded.
newtype ToRecordCodec a = ToRecordCodec (Either a (NonEmptyArray (Tuple (Maybe String) (String -> Maybe Json -> Either (List String) a))))

newtype FromRecordCodec a = FromRecordCodec (Tuple (Maybe String) (String -> a -> Maybe Json))

toStatic :: forall a. a -> ToRecordCodec a
toStatic a = ToRecordCodec $ Left a

fromRequired :: forall a. (a -> Json) -> FromRecordCodec a
fromRequired f = FromRecordCodec $ Tuple Nothing \_ -> Just <<< f

toRequired :: forall a. (Json -> Either (List String) a) -> ToRecordCodec a
toRequired f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> Left $ pure $ "Missing field, " <> show k
    Just j' -> withKey k $ f j'

fromRequiredRename :: forall a. String -> (a -> Json) -> FromRecordCodec a
fromRequiredRename str f = FromRecordCodec $ Tuple (Just str) \_ -> Just <<< f

toRequiredRename :: forall a. String -> (Json -> Either (List String) a) -> ToRecordCodec a
toRequiredRename jsonLbl f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> Left $ pure $ "Missing field, " <> show k
    Just j' -> withKey k $ f j'

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> Json) -> FromRecordCodec (Maybe a)
fromOption f = FromRecordCodec $ Tuple Nothing \_ -> map f

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully tod.
toOption :: forall a. (Json -> Either (List String) a) -> ToRecordCodec (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

fromOptionRename :: forall a. String -> (a -> Json) -> FromRecordCodec (Maybe a)
fromOptionRename str f = FromRecordCodec $ Tuple (Just str) \_ -> map f

toOptionRename :: forall a. String -> (Json -> Either (List String) a) -> ToRecordCodec (Maybe a)
toOptionRename rename f = toOptionDefaultRename rename Nothing (map Just <$> f)

toOptionDefault :: forall a. a -> (Json -> Either (List String) a) -> ToRecordCodec a
toOptionDefault a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> pure a
    Just j' -> withKey k $ f j'

toOptionDefaultRename :: forall a. String -> a -> (Json -> Either (List String) a) -> ToRecordCodec a
toOptionDefaultRename jsonLbl a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> pure a
    Just j' -> withKey k $ f j'

fromOptionArray :: forall a. (a -> Json) -> FromRecordCodec (Array a)
fromOptionArray f = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ fromArray f arr

toOptionArray :: forall a. (Json -> Either (List String) a) -> ToRecordCodec (Array a)
toOptionArray f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> toArray f j'

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> Json) -> FromRecordCodec (Array (Tuple a b))
fromOptionAssocArray k' v' = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

toOptionAssocArray :: forall a b. (String -> Either (List String) a) -> (Json -> Either (List String) b) -> ToRecordCodec (Array (Tuple a b))
toOptionAssocArray k' v' = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> (Object.toUnfoldable <$> toJObject j') >>= traverse (bitraverse k' v')

class ToRecordObj :: RowList Type -> Type -> Type -> Constraint
class ToRecordObj codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> Object Json -> Either (List String) values

instance toRecordObjNil :: ToRecordObj RL.Nil {} {} where
  toRecordObj _ _ _ = pure {}

instance toRecordObjCons ::
  ( ToRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Reflectable sym String
  , Row.Cons sym (ToRecordCodec a) cRest codecs
  , Row.Cons sym a vRest values
  , Row.Lacks sym vRest
  ) =>
  ToRecordObj (RL.Cons sym (ToRecordCodec a) codecTail) { | codecs } { | values } where
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
else instance toRecordObjFailure ::
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'ToRecordCodec a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `toRequired*`/`toOption*` variants.")
          )
      )
  ) =>
  ToRecordObj (RL.Cons sym a codecTail) { | codecs } { | values } where
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
else instance fromRecordObjFailure ::
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
