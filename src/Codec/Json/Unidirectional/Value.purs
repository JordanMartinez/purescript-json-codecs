-- @inline export withAttempts arity=2
-- @inline export altAccumulateLazy arity=2
-- @inline export fromPrimitiveArray(..).fromPrimitive arity=1
-- @inline export fromPrimitiveObject(..).fromPrimitive arity=1
-- @inline export fromPrimitiveRecord(..).fromPrimitive arity=2
-- @inline export fromPrimitiveFailure(..).fromPrimitive always
-- @inline export underIndex' arity=1
-- @inline export underIndex arity=1
-- @inline export underKey arity=1
-- @inline export underKey' arity=1
-- @inline export toIdentity arity=1
-- @inline export toMaybeTagged arity=1
-- @inline export toEither arity=1
-- @inline export toTuple arity=1
-- @inline export toThese arity=1
-- @inline export toNonEmpty arity=1
-- @inline export toList arity=1
-- @inline export toNonEmptyList arity=1
-- @inline export toMap arity=1
-- @inline export toSet arity=1
-- @inline export toNonEmptySet arity=1
-- @inline export toEither arity=1
-- @inline export fromRecord arity=2
-- @inline export toRecord arity=2
-- @inline export fromRecordN arity=3
-- @inline export toRecordN arity=3
-- @inline export toStatic arity=1
-- @inline export fromRequired arity=1
-- @inline export toRequired arity=1
-- @inline export fromRequiredRename arity=2
-- @inline export toRequiredRename arity=2
-- @inline export fromOption arity=1
-- @inline export toOption arity=1
-- @inline export fromOptionRename arity=2
-- @inline export toOptionRename arity=2
-- @inline export toOptionDefault arity=2
-- @inline export toOptionDefaultRename arity=3
-- @inline export fromOptionArray arity=1
-- @inline export toOptionArray arity=1
-- @inline export fromOptionAssocArray arity=2
-- @inline export toOptionAssocArray arity=2
-- @inline export toRecordObjNil(..).toRecordObj always
-- @inline export toRecordObjCons(..).toRecordObj arity=5
-- @inline export toRecordObjFailure(..).fromRecordObj always
-- @inline export fromRecordObjNil(..).fromRecordObj always
-- @inline export fromRecordObjCons(..).fromRecordObj arity=4
-- @inline export fromRecordObjFailure(..).fromRecordObj always
module Codec.Json.Unidirectional.Value
  ( DecodeError(..)
  , accumulateErrors
  , printDecodeError
  , coerce1
  , class FromPrimitive
  , fromPrimitive
  , class AllPrimitive
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
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, codePointAt)
import Data.String as SCP
import Data.String.CodeUnits (charAt)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol, reflectSymbol)
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

data DecodeError
  = AtKey String DecodeError
  | AtIndex Int DecodeError
  | DecodeError String
  | AccumulateError (List DecodeError)

derive instance Eq DecodeError
derive instance Generic DecodeError _
instance Show DecodeError where
  show x = genericShow x

accumulateErrors :: DecodeError -> DecodeError -> DecodeError
accumulateErrors = case _, _ of
  AccumulateError first', next -> AccumulateError $ next : first'
  first, next -> AccumulateError $ next : first : Nil

printDecodeError :: DecodeError -> String
printDecodeError = go 1 "ROOT"
  where
  go indent acc = case _ of
    AtKey k next -> go indent (acc <> "." <> show k) next
    AtIndex i next -> go indent (acc <> "[" <> show i <> "]") next
    AccumulateError ls -> do
      let newlineIndent = "\n" <> power "  " indent
      acc <> (foldMap (go (indent + 1) newlineIndent) $ List.reverse ls)
    DecodeError msg -> acc <> " - " <> msg

withAttempts :: forall a j b. NonEmptyArray a -> (a -> j -> Either DecodeError b) -> j -> Either DecodeError b
withAttempts decoders fn j = go 0
  where
  lastIdx = NEA.length decoders - 1
  go idx = case fn (unsafePartial $ NEA.unsafeIndex decoders idx) j of
    x@(Right _) -> x
    x@(Left _)
      | idx == lastIdx -> x
      | otherwise -> go (idx + 1)

altAccumulateLazy :: forall a. (Json -> Either DecodeError a) -> (Json -> Either DecodeError a) -> Json -> Either DecodeError a
altAccumulateLazy f g j = case f j of
  x@(Right _) -> x
  (Left e1) -> case g j of
    x@(Right _) -> x
    Left e2 -> Left $ accumulateErrors e1 e2

class FromPrimitive :: Type -> Constraint
class FromPrimitive a where
  fromPrimitive :: a -> Json

instance FromPrimitive Boolean where
  fromPrimitive = unsafeCoerce
else instance FromPrimitive Number where
  fromPrimitive = unsafeCoerce
else instance FromPrimitive String where
  fromPrimitive = unsafeCoerce
else instance fromPrimitiveArray :: FromPrimitive a => FromPrimitive (Array a) where
  fromPrimitive = unsafeCoerce
else instance fromPrimitiveObject :: FromPrimitive a => FromPrimitive (Object a) where
  fromPrimitive = unsafeCoerce
else instance fromPrimitiveRecord ::
  ( RowToList rows rl
  , AllPrimitive rl
  ) =>
  FromPrimitive { | rows } where
  fromPrimitive = unsafeCoerce
else instance fromPrimitiveFailure ::
  ( Fail (Beside (Text "Expected a primitive JSON type but got type: ") (Quote a))
  ) =>
  FromPrimitive a where
  fromPrimitive _ = unsafeCrashWith "Impossible"

class AllPrimitive :: RL.RowList Type -> Constraint
class AllPrimitive rl

instance allPrimitiveNil :: AllPrimitive RL.Nil
instance allPrimitiveCons :: (AllPrimitive tail, FromPrimitive a) => AllPrimitive (RL.Cons sym a tail)

fromVoid :: Void -> Json
fromVoid = absurd

toVoid :: Json -> Either DecodeError Void
toVoid _ = Left $ DecodeError $ "Decoding a value to Void is impossible"

coerce1 :: forall n a. Coercible n a => (a -> n) -> Either DecodeError a -> Either DecodeError n
coerce1 _ = coerce

fromJNull :: Json
fromJNull = Json.jsonNull

fromUnit :: Unit -> Json
fromUnit = const fromJNull

toJNull :: Json -> Either DecodeError Unit
toJNull json =
  caseJson
    pure
    (\_ -> Left $ DecodeError $ "Expected a value of type Null but got Boolean")
    (\_ -> Left $ DecodeError $ "Expected a value of type Null but got Number")
    (\_ -> Left $ DecodeError $ "Expected a value of type Null but got String")
    (\_ -> Left $ DecodeError $ "Expected a value of type Null but got Array")
    (\_ -> Left $ DecodeError $ "Expected a value of type Null but got Object")
    json

toNullDefaultOrA :: forall a. a -> (Json -> Either DecodeError a) -> Json -> Either DecodeError a
toNullDefaultOrA def f = altAccumulateLazy (\j -> def <$ toJNull j) f

fromNullNothingOrJust :: forall a. (a -> Json) -> Maybe a -> Json
fromNullNothingOrJust f = maybe Json.jsonNull f

toNullNothingOrJust :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

fromNullable :: forall a. (a -> Json) -> Nullable a -> Json
fromNullable fromA = toMaybe >>> fromNullNothingOrJust fromA

toNullable :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Nullable a)
toNullable toA = altAccumulateLazy (\j -> null <$ toJNull j) (\j -> notNull <$> toA j)

fromBoolean :: Boolean -> Json
fromBoolean = Json.fromBoolean

toBoolean :: Json -> Either DecodeError Boolean
toBoolean json =
  caseJson
    (\_ -> Left $ DecodeError $ "Expected a value of type Boolean but got Null")
    pure
    (\_ -> Left $ DecodeError $ "Expected a value of type Boolean but got Number")
    (\_ -> Left $ DecodeError $ "Expected a value of type Boolean but got String")
    (\_ -> Left $ DecodeError $ "Expected a value of type Boolean but got Array")
    (\_ -> Left $ DecodeError $ "Expected a value of type Boolean but got Object")
    json

fromNumber :: Number -> Json
fromNumber = Json.fromNumber

toNumber :: Json -> Either DecodeError Number
toNumber json =
  caseJson
    (\_ -> Left $ DecodeError $ "Expected a value of type Number but got Null")
    (\_ -> Left $ DecodeError $ "Expected a value of type Number but got Boolean")
    pure
    (\_ -> Left $ DecodeError $ "Expected a value of type Number but got String")
    (\_ -> Left $ DecodeError $ "Expected a value of type Number but got Array")
    (\_ -> Left $ DecodeError $ "Expected a value of type Number but got Object")
    json

fromInt :: Int -> Json
fromInt = Int.toNumber >>> fromNumber

fromString :: String -> Json
fromString = Json.fromString

toInt :: Json -> Either DecodeError Int
toInt = toNumber >=> (\n -> note (DecodeError "Could not convert Number to Int") $ Int.fromNumber n)

toString :: Json -> Either DecodeError String
toString json =
  caseJson
    (\_ -> Left $ DecodeError $ "Expected a value of type String but got Null")
    (\_ -> Left $ DecodeError $ "Expected a value of type String but got Boolean")
    (\_ -> Left $ DecodeError $ "Expected a value of type String but got Number")
    pure
    (\_ -> Left $ DecodeError $ "Expected a value of type String but got Array")
    (\_ -> Left $ DecodeError $ "Expected a value of type String but got Object")
    json

fromChar :: Char -> Json
fromChar = SCU.singleton >>> fromString

toChar :: Json -> Either DecodeError Char
toChar = toString >=> (note (DecodeError "Could not get char at index 0 in String") <<< charAt 0)

fromCodePoint :: CodePoint -> Json
fromCodePoint = SCP.singleton >>> fromString

toCodePoint :: Json -> Either DecodeError CodePoint
toCodePoint = toString >=> (\s -> note (DecodeError "Could not get code point from String") $ codePointAt 0 s)

fromNonEmptyString :: NonEmptyString -> Json
fromNonEmptyString (NonEmptyString s) = fromString s

toNonEmptyString :: Json -> Either DecodeError NonEmptyString
toNonEmptyString = toString >=> (note (DecodeError "Received empty string") <<< NonEmptyString.fromString)

fromJArray :: Array Json -> Json
fromJArray = Json.fromArray

toJArray :: Json -> Either DecodeError (Array Json)
toJArray json =
  caseJson
    (\_ -> Left $ DecodeError $ "Expected a value of type Array but got Null")
    (\_ -> Left $ DecodeError $ "Expected a value of type Array but got Boolean")
    (\_ -> Left $ DecodeError $ "Expected a value of type Array but got Number")
    (\_ -> Left $ DecodeError $ "Expected a value of type Array but got String")
    pure
    (\_ -> Left $ DecodeError $ "Expected a value of type Array but got Object")
    json

underIndex :: forall a. Int -> (Json -> Either DecodeError a) -> Array Json -> Either DecodeError a
underIndex idx f arr = case Array.index arr idx of
  Nothing -> Left $ AtIndex idx $ DecodeError "Missing index"
  Just j -> lmap (AtIndex idx) $ f j

fromArray :: forall a. (a -> Json) -> Array a -> Json
fromArray fromA = map fromA >>> fromJArray

toArray :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> lmap (AtIndex i) $ toElem j)

fromArray2 :: Json -> Json -> Json
fromArray2 a b = fromJArray [ a, b ]

toArray2
  :: forall a b x
   . (a -> b -> x)
  -> (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError x
toArray2 x a' b' = toJArray >=> case _ of
  [ a, b ] ->
    x
      <$> a' a
      <*> b' b
  _ -> Left $ DecodeError "Expected array of length 2"

fromArray3 :: Json -> Json -> Json -> Json
fromArray3 a b c = fromJArray [ a, b, c ]

toArray3
  :: forall a b c x
   . (a -> b -> c -> x)
  -> (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> Json
  -> Either DecodeError x
toArray3 x a' b' c' = toJArray >=> case _ of
  [ a, b, c ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
  _ -> Left $ DecodeError "Expected array of length 3"

fromArray4 :: Json -> Json -> Json -> Json -> Json
fromArray4 a b c d = fromJArray [ a, b, c, d ]

toArray4
  :: forall a b c d x
   . (a -> b -> c -> d -> x)
  -> (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> (Json -> Either DecodeError d)
  -> Json
  -> Either DecodeError x
toArray4 x a' b' c' d' = toJArray >=> case _ of
  [ a, b, c, d ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
  _ -> Left $ DecodeError "Expected array of length 4"

fromArray5 :: Json -> Json -> Json -> Json -> Json -> Json
fromArray5 a b c d e = fromJArray [ a, b, c, d, e ]

toArray5
  :: forall a b c d e x
   . (a -> b -> c -> d -> e -> x)
  -> (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> (Json -> Either DecodeError d)
  -> (Json -> Either DecodeError e)
  -> Json
  -> Either DecodeError x
toArray5 x a' b' c' d' e' = toJArray >=> case _ of
  [ a, b, c, d, e ] ->
    x
      <$> a' a
      <*> b' b
      <*> c' c
      <*> d' d
      <*> e' e
  _ -> Left $ DecodeError "Expected array of length 5"

fromNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
fromNonEmptyArray fromA = NEA.toArray >>> fromArray fromA

toNonEmptyArray :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (NonEmptyArray a)
toNonEmptyArray toElem = toArray toElem >=> (note (DecodeError "Received empty array") <<< NEA.fromArray)

fromJObject :: Object Json -> Json
fromJObject = Json.fromObject

toJObject :: Json -> Either DecodeError (Object Json)
toJObject json =
  caseJson
    (\_ -> Left $ DecodeError "Expected a value of type Object but got Null")
    (\_ -> Left $ DecodeError "Expected a value of type Object but got Boolean")
    (\_ -> Left $ DecodeError "Expected a value of type Object but got Number")
    (\_ -> Left $ DecodeError "Expected a value of type Object but got String")
    (\_ -> Left $ DecodeError "Expected a value of type Object but got Array")
    pure
    json

underKey :: forall a. String -> (Json -> Either DecodeError a) -> Object Json -> Either DecodeError a
underKey key f obj = case Object.lookup key obj of
  Nothing -> Left $ AtKey key $ DecodeError "Missing key"
  Just j -> lmap (AtKey key) $ f j

fromObject :: forall a. (a -> Json) -> Object a -> Json
fromObject fromA = map fromA >>> fromJObject

toObject :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Object a)
toObject toElem = toJObject >=> traverseWithIndex (\k j -> lmap (AtKey k) $ toElem j)

fromObjSingleton :: String -> Json -> Json
fromObjSingleton k v = fromJObject $ Object.singleton k v

toObjSingleton :: forall a. String -> (Json -> Either DecodeError a) -> Json -> Either DecodeError a
toObjSingleton k f = toJObject >=> (\j -> underKey k f j)

fromPropArray :: Array (Tuple String Json) -> Json
fromPropArray = fromJObject <<< Array.foldl (\acc (Tuple k v) -> Object.insert k v acc) Object.empty

fromIdentity :: forall a. (a -> Json) -> Identity a -> Json
fromIdentity fromA (Identity a) = fromA a

toIdentity :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Identity a)
toIdentity f = coerce f

fromMaybeTagged :: forall a. (a -> Json) -> Maybe a -> Json
fromMaybeTagged fromA =
  maybe (Object.singleton "tag" $ fromString "Nothing") (tagged "Just" <<< fromA)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toMaybeTagged :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Maybe a)
toMaybeTagged toElem = toJObject >=> \jo -> do
  tag <- jo # underKey "tag" toString
  case tag of
    "Just" -> (map Just <$> underKey "value" toElem) jo
    "Nothing" ->
      pure Nothing
    _ ->
      Left $ DecodeError "Tag was not 'Just' or 'Nothing'."

fromEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEither fromA fromB =
  either (tagged "Left" <<< fromA) (tagged "Right" <<< fromB)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toEither
  :: forall a b
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError (Either a b)
toEither toLeft toRight = toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "Left" -> Left <$> underKey "value" toLeft jo
    "Right" -> Right <$> underKey "value" toRight jo
    _ ->
      Left $ DecodeError "Tag was not 'Left' or 'Right'"

fromTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
fromTuple fromA fromB (Tuple a b) =
  fromJArray
    [ fromA a
    , fromB b
    ]

toTuple
  :: forall a b
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError (Tuple a b)
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
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError (These a b)
toThese toA toB = toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "This" -> This <$> underKey "value" toA jo
    "That" -> That <$> underKey "value" toB jo
    "Both" ->
      Both
        <$> (underKey "this" toA jo)
        <*> (underKey "that" toB jo)
    _ ->
      Left $ DecodeError "Tag was not 'This', 'That', or 'Both'"

fromNonEmpty :: forall f a. (a -> Json) -> (f a -> Json) -> NonEmpty f a -> Json
fromNonEmpty fromHead fromTail (NonEmpty a fa) =
  fromJObject
    $ Object.fromFoldable
        [ Tuple "head" $ fromHead a
        , Tuple "tail" $ fromTail fa
        ]

toNonEmpty
  :: forall g a
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError (g a))
  -> (Json -> Either DecodeError (NonEmpty g a))
toNonEmpty toHead toTail = toJObject >=> \jo ->
  NonEmpty
    <$> (underKey "head" toHead jo)
    <*> (underKey "tail" toTail jo)

fromList :: forall a. (a -> Json) -> List a -> Json
fromList fromA = List.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toList
  :: forall a
   . (Json -> Either DecodeError a)
  -> Json
  -> Either DecodeError (List a)
toList toElem = toArray toElem >>> map List.fromFoldable

fromNonEmptyList :: forall a. (a -> Json) -> (NonEmptyList a -> Json)
fromNonEmptyList fromA = NEL.foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

toNonEmptyList
  :: forall a
   . (Json -> Either DecodeError a)
  -> Json
  -> Either DecodeError (NonEmptyList a)
toNonEmptyList toA = toList toA >=> (note (DecodeError "Received empty list") <<< NEL.fromList)

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
  => (Json -> Either DecodeError k)
  -> (Json -> Either DecodeError v)
  -> Json
  -> Either DecodeError (Map k v)
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
  => (Json -> Either DecodeError a)
  -> Json
  -> Either DecodeError (Set a)
toSet toA = toArray toA >>> map Set.fromFoldable

fromNonEmptySet :: forall a. (a -> Json) -> NonEmptySet a -> Json
fromNonEmptySet fromA = NonEmptySet.toSet >>> fromSet fromA

toNonEmptySet
  :: forall a
   . Ord a
  => (Json -> Either DecodeError a)
  -> Json
  -> Either DecodeError (NonEmptySet a)
toNonEmptySet toA = toArray toA >=> (note (DecodeError "Received empty set") <<< NonEmptySet.fromSet <<< Set.fromFoldable)

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
  -> Either DecodeError { | values }
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
  -> Either DecodeError n
toRecordN f codecs = coerce1 f <<< toRecord codecs

-- | Iterates through the underlying array.
-- | - key: uses the `str` in `Just str` or the record label if `Nothing`
-- | - f: the key used on the object and the result of looking up that key in the object
-- | - return: `Nothing` if the decoding failed; `Just` if it succeeded.
newtype ToRecordCodec a = ToRecordCodec (Either a (NonEmptyArray (Tuple (Maybe String) (String -> Maybe Json -> Either DecodeError a))))

newtype FromRecordCodec a = FromRecordCodec (Tuple (Maybe String) (String -> a -> Maybe Json))

toStatic :: forall a. a -> ToRecordCodec a
toStatic a = ToRecordCodec $ Left a

fromRequired :: forall a. (a -> Json) -> FromRecordCodec a
fromRequired f = FromRecordCodec $ Tuple Nothing \_ -> Just <<< f

toRequired :: forall a. (Json -> Either DecodeError a) -> ToRecordCodec a
toRequired f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> Left $ DecodeError $ "Missing field, " <> show k
    Just j' -> lmap (AtKey k) $ f j'

fromRequiredRename :: forall a. String -> (a -> Json) -> FromRecordCodec a
fromRequiredRename str f = FromRecordCodec $ Tuple (Just str) \_ -> Just <<< f

toRequiredRename :: forall a. String -> (Json -> Either DecodeError a) -> ToRecordCodec a
toRequiredRename jsonLbl f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> Left $ DecodeError $ "Missing field, " <> show k
    Just j' -> lmap (AtKey k) $ f j'

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> Json) -> FromRecordCodec (Maybe a)
fromOption f = FromRecordCodec $ Tuple Nothing \_ -> map f

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully tod.
toOption :: forall a. (Json -> Either DecodeError a) -> ToRecordCodec (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

fromOptionRename :: forall a. String -> (a -> Json) -> FromRecordCodec (Maybe a)
fromOptionRename str f = FromRecordCodec $ Tuple (Just str) \_ -> map f

toOptionRename :: forall a. String -> (Json -> Either DecodeError a) -> ToRecordCodec (Maybe a)
toOptionRename rename f = toOptionDefaultRename rename Nothing (map Just <$> f)

toOptionDefault :: forall a. a -> (Json -> Either DecodeError a) -> ToRecordCodec a
toOptionDefault a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \k j ->
  case j of
    Nothing -> pure a
    Just j' -> lmap (AtKey k) $ f j'

toOptionDefaultRename :: forall a. String -> a -> (Json -> Either DecodeError a) -> ToRecordCodec a
toOptionDefaultRename jsonLbl a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) \k j ->
  case j of
    Nothing -> pure a
    Just j' -> lmap (AtKey k) $ f j'

fromOptionArray :: forall a. (a -> Json) -> FromRecordCodec (Array a)
fromOptionArray f = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ fromArray f arr

toOptionArray :: forall a. (Json -> Either DecodeError a) -> ToRecordCodec (Array a)
toOptionArray f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> toArray f j'

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> Json) -> FromRecordCodec (Array (Tuple a b))
fromOptionAssocArray k' v' = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

toOptionAssocArray :: forall a b. (String -> Either DecodeError a) -> (Json -> Either DecodeError b) -> ToRecordCodec (Array (Tuple a b))
toOptionAssocArray k' v' = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> pure []
  Just j' -> (Object.toUnfoldable <$> toJObject j') >>= traverse (bitraverse k' v')

class ToRecordObj :: RowList Type -> Type -> Type -> Constraint
class ToRecordObj codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> Object Json -> Either DecodeError values

instance toRecordObjNil :: ToRecordObj RL.Nil {} {} where
  toRecordObj _ _ _ = pure {}

instance toRecordObjCons ::
  ( ToRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Row.Cons sym (ToRecordCodec a) cRest codecs
  , Row.Cons sym a vRest values
  , Row.Lacks sym vRest
  ) =>
  ToRecordObj (RL.Cons sym (ToRecordCodec a) codecTail) { | codecs } { | values } where
  toRecordObj _ codecs j = do
    rec <- onLeft (toRecordObj (Proxy :: Proxy codecTail) codecsRest j) \e1 ->
      case keyDecoders of
        Right decs | Left e2 <- decodeNextField decs -> Left $ accumulateErrors e1 e2
        _ -> Left e1
    a <- case keyDecoders of
      Left a' -> pure a'
      Right decs -> decodeNextField decs
    pure $ Record.insert _lbl a rec
    where
    onLeft :: forall e x. Either e x -> (e -> Either e x) -> Either e x
    onLeft l f = case l of
      Left l' -> f l'
      x@(Right _) -> x

    decodeNextField :: NonEmptyArray (Tuple (Maybe String) (String -> Maybe Json -> Either DecodeError a)) -> Either DecodeError a
    decodeNextField decoders =
      j # withAttempts decoders \(Tuple keyRename decoder) j' -> do
        let key = fromMaybe lbl keyRename
        decoder key (Object.lookup key j')
    lbl = reflectSymbol _lbl
    _lbl = (Proxy :: Proxy sym)
    (ToRecordCodec keyDecoders) = Record.get _lbl codecs

    codecsRest :: { | cRest }
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
    lbl = reflectSymbol _lbl
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
