-- | Unidirectional, value-based JSON codecs.
-- | This module should be imported using a qualified `J` or `Json` alias:
-- | ```
-- | import Codec.Json.Unidirectional.Value as J
-- | import Codec.Json.Unidirectional.Value as Json
-- | ```
-- | thereby causing `to*` and `from*` code to read like so:
-- | - `J.fromInt`/`Json.fromInt`, which reads "encode an `Int` to `Json`"
-- | - `J.toInt`/`Json.toInt`, which reads "decode `JSON` to an `Int`"
--
-- @inline export fromPrimitiveArray(..).fromPrimitive arity=1
-- @inline export fromPrimitiveObject(..).fromPrimitive arity=1
-- @inline export fromPrimitiveRecord(..).fromPrimitive arity=2
-- @inline export fromPrimitiveFailure(..).fromPrimitive always
-- @inline export underIndex arity=1
-- @inline export underKey arity=1
-- @inline export toIdentity arity=1
-- @inline export fromRecord arity=4
-- @inline export toRecord arity=3
-- @inline export fromRecordN arity=4
-- @inline export toRecordN arity=3
-- @inline export toStatic arity=1
-- @inline export fromRequired arity=1
-- @inline export fromRequired' arity=1
-- @inline export fromRequiredRename arity=2
-- @inline export fromRequiredRename' arity=2
-- @inline export fromOption arity=2
-- @inline export fromOption' arity=2
-- @inline export fromOptionRename arity=3
-- @inline export fromOptionRename' arity=3
-- @inline export fromOptionArray arity=2
-- @inline export fromOptionArray' arity=2
-- @inline export fromOptionAssocArray arity=3
-- @inline export fromOptionAssocArray' arity=3
-- @inline export fromRecordObjCons arity=4
-- @inline export fromRecordObjCons(..).fromRecordObj arity=3
module Codec.Json.Unidirectional.Value
  ( DecodeError(..)
  , printDecodeError
  , printDecodeError'
  , coerce1
  , class FromPrimitive
  , fromPrimitive
  , class AllPrimitive
  , fromVoid
  , toVoid
  , fromJNull
  , fromNull
  , toJNull
  , fromUnit
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
  , fromEitherTagged
  , toEitherTagged
  , fromEitherSingle
  , toEitherSingle
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
  , FromProp(..)
  , ToProp(..)
  , toStatic
  , fromRequired
  , fromRequired'
  , toRequired
  , fromRequiredRename
  , fromRequiredRename'
  , toRequiredRename
  , fromOption
  , fromOption'
  , toOption
  , fromOptionRename
  , fromOptionRename'
  , toOptionRename
  , toOptionDefault
  , toOptionDefaultRename
  , fromOptionArray
  , fromOptionArray'
  , toOptionArray
  , fromOptionAssocArray
  , fromOptionAssocArray'
  , toOptionAssocArray
  , class ToRecordObj
  , toRecordObj
  , class FromRecordObj
  , fromRecordObj
  ) where

import Prelude

import Codec.Json.Unidirectional.Value.SortRowList (class IntThenAlphaSortedRowList, class ToOrdering)
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Generic.Rep (class Generic)
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

-- | An error type that 
-- | - includes path-to-json information
-- | - allows for custom decode messages
data DecodeError
  = AtKey String DecodeError
  | AtIndex Int DecodeError
  | DecodeError String

derive instance Eq DecodeError
derive instance Generic DecodeError _
instance Show DecodeError where
  show x = genericShow x

-- | Pretty-prints the decode error over a multi-line string.
printDecodeError :: DecodeError -> String
printDecodeError = printDecodeError' "ROOT"

printDecodeError' :: String -> DecodeError -> String
printDecodeError' acc = case _ of
  DecodeError msg ->
    acc <> " - " <> msg
  AtKey k next ->
    printDecodeError' (acc <> "." <> show k) next
  AtIndex i next ->
    printDecodeError' (acc <> "[" <> show i <> "]") next

-- | Indicates which values are primitive JSON values that can be encoded via `unsafeCoerce`.
class FromPrimitive :: Type -> Constraint
class FromPrimitive a where
  -- | Shortcut encoder for encoding primitive JSON values.
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

-- | Utility class that distinguishes which records are primitive and which are not.
-- | Used in `FromPrimitive`.
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

fromNull :: forall a. a -> Json
fromNull = const fromJNull

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
toNullDefaultOrA def f j = (def <$ toJNull j) <|> f j

fromNullNothingOrJust :: forall a. (a -> Json) -> Maybe a -> Json
fromNullNothingOrJust f = maybe Json.jsonNull f

toNullNothingOrJust :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

fromNullable :: forall a. (a -> Json) -> Nullable a -> Json
fromNullable fromA = toMaybe >>> fromNullNothingOrJust fromA

toNullable :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Nullable a)
toNullable toA j = (null <$ toJNull j) <|> (notNull <$> toA j)

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
underIndex idx f arr = lmap (AtIndex idx) case Array.index arr idx of
  Nothing -> Left $ DecodeError "Missing index"
  Just j -> f j

fromArray :: forall a. (a -> Json) -> Array a -> Json
fromArray fromA = map fromA >>> fromJArray

toArray :: forall a. (Json -> Either DecodeError a) -> Json -> Either DecodeError (Array a)
toArray toElem = toJArray >=> traverseWithIndex (\i j -> lmap (AtIndex i) $ toElem j)

fromArray2 :: Json -> Json -> Json
fromArray2 a b = fromJArray [ a, b ]

toArray2
  :: forall a b x
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (a -> b -> x)
  -> Json
  -> Either DecodeError x
toArray2 a' b' x = toJArray >=> case _ of
  [ a, b ] ->
    x
      <$> (lmap (AtIndex 0) $ a' a)
      <*> (lmap (AtIndex 1) $ b' b)
  _ -> Left $ DecodeError "Expected array of length 2"

fromArray3 :: Json -> Json -> Json -> Json
fromArray3 a b c = fromJArray [ a, b, c ]

toArray3
  :: forall a b c x
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> (a -> b -> c -> x)
  -> Json
  -> Either DecodeError x
toArray3 a' b' c' x = toJArray >=> case _ of
  [ a, b, c ] ->
    x
      <$> (lmap (AtIndex 0) $ a' a)
      <*> (lmap (AtIndex 1) $ b' b)
      <*> (lmap (AtIndex 2) $ c' c)
  _ -> Left $ DecodeError "Expected array of length 3"

fromArray4 :: Json -> Json -> Json -> Json -> Json
fromArray4 a b c d = fromJArray [ a, b, c, d ]

toArray4
  :: forall a b c d x
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> (Json -> Either DecodeError d)
  -> (a -> b -> c -> d -> x)
  -> Json
  -> Either DecodeError x
toArray4 a' b' c' d' x = toJArray >=> case _ of
  [ a, b, c, d ] ->
    x
      <$> (lmap (AtIndex 0) $ a' a)
      <*> (lmap (AtIndex 1) $ b' b)
      <*> (lmap (AtIndex 2) $ c' c)
      <*> (lmap (AtIndex 3) $ d' d)
  _ -> Left $ DecodeError "Expected array of length 4"

fromArray5 :: Json -> Json -> Json -> Json -> Json -> Json
fromArray5 a b c d e = fromJArray [ a, b, c, d, e ]

toArray5
  :: forall a b c d e x
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> (Json -> Either DecodeError c)
  -> (Json -> Either DecodeError d)
  -> (Json -> Either DecodeError e)
  -> (a -> b -> c -> d -> e -> x)
  -> Json
  -> Either DecodeError x
toArray5 a' b' c' d' e' x = toJArray >=> case _ of
  [ a, b, c, d, e ] ->
    x
      <$> (lmap (AtIndex 0) $ a' a)
      <*> (lmap (AtIndex 1) $ b' b)
      <*> (lmap (AtIndex 2) $ c' c)
      <*> (lmap (AtIndex 3) $ d' d)
      <*> (lmap (AtIndex 4) $ e' e)
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
underKey key f obj = lmap (AtKey key) case Object.lookup key obj of
  Nothing -> Left $ DecodeError "Missing key"
  Just j -> f j

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

fromEitherTagged :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEitherTagged fromA fromB =
  either (tagged "Left" <<< fromA) (tagged "Right" <<< fromB)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

toEitherTagged
  :: forall a b
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError (Either a b)
toEitherTagged toLeft toRight = toJObject >=> \jo -> do
  tag <- underKey "tag" toString jo
  case tag of
    "Left" -> Left <$> underKey "value" toLeft jo
    "Right" -> Right <$> underKey "value" toRight jo
    _ ->
      Left $ DecodeError "Tag was not 'Left' or 'Right'"

fromEitherSingle :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEitherSingle fromA fromB =
  either (fromObjSingleton "Left" <<< fromA) (fromObjSingleton "Right" <<< fromB)

toEitherSingle
  :: forall a b
   . (Json -> Either DecodeError a)
  -> (Json -> Either DecodeError b)
  -> Json
  -> Either DecodeError (Either a b)
toEitherSingle toLeft toRight j =
  (Left <$> toObjSingleton "Left" toLeft j) <|> (Right <$> toObjSingleton "Right" toRight j)

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
    toA
    toB
    Tuple

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

-- | All labels must have a value of type `FromProp a`,
-- | which can be obtained via functions like `fromRequired*` and `fromOption*`
-- | or by defining a value yourself. Otherwise, you will get a compiler error:
-- |
-- | ```
-- | fromRecord
-- |   { label: fromRequired $ fromInt 
-- |   , psName: fromRequiredRename "jsonName" fromInt
-- |   , optionalDisappears: fromOption $ fromInt
-- |   , optionalStillThere: fromOption $ fromInt
-- |   }
-- |   { label: 1
-- |   , psName: Just 2
-- |   , optionalDisappears: Nothing
-- |   , optionalStillThere: Just 3
-- |   }
-- | ```
-- | produces the following JSON
-- | ```
-- | { "label": 1
-- | , "jsonName": 2
-- | , "optionalStillThere": 3
-- | }
-- | ```
fromRecord
  :: forall codecs values alphaSortedCodecsRl insertOrderThenAlphaSortedCodecsRL
   . RowToList codecs alphaSortedCodecsRl
  => IntThenAlphaSortedRowList alphaSortedCodecsRl insertOrderThenAlphaSortedCodecsRL
  => FromRecordObj insertOrderThenAlphaSortedCodecsRL { | codecs } { | values }
  => { | codecs }
  -> { | values }
  -> Json
fromRecord codecs values = fromJObject $
  fromRecordObj (Proxy :: Proxy insertOrderThenAlphaSortedCodecsRL) codecs values

-- | All labels must have a value of type `ToProp a`,
-- | which can be obtained via functions like `toRequired*` and `toOption*`
-- | or by defining a value yourself. Otherwise, you will get a compiler error:
-- |
-- | The following JSON and codec...
-- | ```
-- | toRecord
-- |   { label: toRequired toInt 
-- |   , psName: toRequiredRename "jsonName" toInt
-- |   , optionalAppears: toOption toInt
-- |   , optionalAlwaysThere: toOption toInt
-- |   }
-- |   $ either (\_ -> unsafeCrashWith "error") identity
-- |   $ jsonParser
-- |   """{ "label": 1
-- |      , "jsonName": 2
-- |      , "optionalAlwaysThere": 3
-- |      }"""
-- | ```
-- | ...produces the following value
-- | ```
-- |   { label: 1
-- |   , psName: Just 2
-- |   , optionalAppears: Nothing
-- |   , optionalAlwaysThere: Just 3
-- |   }
-- | ```
toRecord
  :: forall codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj codecsRL { | codecs } { | values }
  => { | codecs }
  -> Json
  -> Either DecodeError { | values }
toRecord codecs = toJObject >=>
  toRecordObj (Proxy :: Proxy codecsRL) codecs

-- | Same as `fromRecord` but handles the Newtype for you
-- | so you don't need to add a type annotation to help
-- | type inference.
fromRecordN
  :: forall n codecs values alphaSortedCodecsRl insertOrderThenAlphaSortedCodecsRL
   . RowToList codecs alphaSortedCodecsRl
  => IntThenAlphaSortedRowList alphaSortedCodecsRl insertOrderThenAlphaSortedCodecsRL
  => FromRecordObj insertOrderThenAlphaSortedCodecsRL { | codecs } { | values }
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> n
  -> Json
fromRecordN _ codecs = unwrap >>> fromRecord codecs

-- | Same as `toRecord` but handles the Newtype for you.
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

-- | Converts a value in a JSON object into a value
-- | associated with the record label.
-- |
-- | Explanation of arguments
-- | - `String -> Maybe Json`: Looks up the provided key in the object. Implemented via `\str -> Object.lookup str obj`
-- | - `String` -> the label of the record
newtype ToProp a = ToProp (Fn2 (String -> Maybe Json) String (Either DecodeError a))

-- | Converts a value associated with the record label
-- | into a JSON value associated with the given label in the JSON object.
-- |
-- | On `Nothing`, the key-value pair is not added to the JSON object.
-- | On `Just`, both the JSON-encoded value and the key to use in the JSON object is provided.
-- |
-- | The `Just`-wrapped `Tuple` is a key-value pair. For the key,
-- | `Nothing` means the label associated with the record is used
-- | while using `Just` means the provided key is used (e.g. renaming).
newtype FromProp :: Int -> Type -> Type
newtype FromProp insertionOrder a = FromProp (a -> Maybe (Tuple (Maybe String) Json))

instance ToOrdering (FromProp insertionOrder a) insertionOrder

type FromPropDefaultOrder a = FromProp 2147483647 a

toStatic :: forall a. a -> ToProp a
toStatic a = ToProp $ mkFn2 \_ _ -> pure a

fromRequired :: forall a. (a -> Json) -> FromPropDefaultOrder a
fromRequired f = FromProp $ (Just <<< Tuple Nothing) <$> f

fromRequired' :: forall @i a. (a -> Json) -> FromProp i a
fromRequired' f = FromProp $ (Just <<< Tuple Nothing) <$> f

toRequired :: forall a. (Json -> Either DecodeError a) -> ToProp a
toRequired f = ToProp $ mkFn2 \lookupFn recLabel ->
  lmap (AtKey recLabel) case lookupFn recLabel of
    Nothing -> Left $ DecodeError $ "Missing field"
    Just j' -> f j'

fromRequiredRename :: forall a. String -> (a -> Json) -> FromPropDefaultOrder a
fromRequiredRename str f = FromProp $ (Just <<< Tuple (Just str)) <$> f

fromRequiredRename' :: forall @i a. String -> (a -> Json) -> FromProp i a
fromRequiredRename' str f = FromProp $ (Just <<< Tuple (Just str)) <$> f

toRequiredRename :: forall a. String -> (Json -> Either DecodeError a) -> ToProp a
toRequiredRename jsonLbl f = ToProp $ mkFn2 \lookupFn _ ->
  lmap (AtKey jsonLbl) case lookupFn jsonLbl of
    Nothing -> Left $ DecodeError "Missing field"
    Just j' -> f j'

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> Json) -> FromPropDefaultOrder (Maybe a)
fromOption f = FromProp $ map (Tuple Nothing <<< f)

fromOption' :: forall @i a. (a -> Json) -> FromProp i (Maybe a)
fromOption' f = FromProp $ map (Tuple Nothing <<< f)

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully tod.
toOption :: forall a. (Json -> Either DecodeError a) -> ToProp (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

fromOptionRename :: forall a. String -> (a -> Json) -> FromPropDefaultOrder (Maybe a)
fromOptionRename str f = FromProp $ map (Tuple (Just str) <<< f)

fromOptionRename' :: forall @i a. String -> (a -> Json) -> FromProp i (Maybe a)
fromOptionRename' str f = FromProp $ map (Tuple (Just str) <<< f)

toOptionRename :: forall a. String -> (Json -> Either DecodeError a) -> ToProp (Maybe a)
toOptionRename rename f = toOptionDefaultRename rename Nothing (map Just <$> f)

toOptionDefault :: forall a. a -> (Json -> Either DecodeError a) -> ToProp a
toOptionDefault a f = ToProp $ mkFn2 \lookupFn recLabel ->
  case lookupFn recLabel of
    Nothing -> pure a
    Just j' -> lmap (AtKey recLabel) $ f j'

toOptionDefaultRename :: forall a. String -> a -> (Json -> Either DecodeError a) -> ToProp a
toOptionDefaultRename jsonLbl a f = ToProp $ mkFn2 \lookupFn _ ->
  case lookupFn jsonLbl of
    Nothing -> pure a
    Just j' -> lmap (AtKey jsonLbl) $ f j'

fromOptionArray :: forall a. (a -> Json) -> FromPropDefaultOrder (Array a)
fromOptionArray f = FromProp $ \arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Tuple Nothing $ fromArray f arr

fromOptionArray' :: forall @i a. (a -> Json) -> FromProp i (Array a)
fromOptionArray' f = FromProp $ \arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Tuple Nothing $ fromArray f arr

toOptionArray :: forall a. (Json -> Either DecodeError a) -> ToProp (Array a)
toOptionArray f = ToProp $ mkFn2 \lookupFn recLabel ->
  case lookupFn recLabel of
    Nothing -> pure []
    Just j' -> lmap (AtKey recLabel) $ toArray f j'

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> Json) -> FromPropDefaultOrder (Array (Tuple a b))
fromOptionAssocArray k' v' = FromProp $ \arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Tuple Nothing $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

fromOptionAssocArray' :: forall @i a b. (a -> String) -> (b -> Json) -> FromProp i (Array (Tuple a b))
fromOptionAssocArray' k' v' = FromProp $ \arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Tuple Nothing $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

toOptionAssocArray :: forall a b. (String -> Either DecodeError a) -> (Json -> Either DecodeError b) -> ToProp (Array (Tuple a b))
toOptionAssocArray k' v' = ToProp $ mkFn2 \lookupFn recLabel ->
  case lookupFn recLabel of
    Nothing -> pure []
    Just j' -> lmap (AtKey recLabel) $
      ( (Object.toUnfoldable <$> toJObject j') >>= traverse \(Tuple k v) ->
          lmap (AtKey k) do
            Tuple
              <$>
                ( k' k # lmap case _ of
                    DecodeError err -> DecodeError $ "while decoding the key " <> show k <> " - " <> err
                    x -> x
                )
              <*> v' v
      )

class ToRecordObj :: RowList Type -> Type -> Type -> Constraint
class ToRecordObj codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> Object Json -> Either DecodeError values

instance toRecordObjNil :: ToRecordObj RL.Nil {} {} where
  toRecordObj _ _ _ = pure {}

instance toRecordObjCons ::
  ( ToRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Row.Cons sym (ToProp a) cRest codecs
  , Row.Cons sym a vRest values
  , Row.Lacks sym vRest
  ) =>
  ToRecordObj (RL.Cons sym (ToProp a) codecTail) { | codecs } { | values } where
  toRecordObj _ codecs j = do
    rec <- toRecordObj (Proxy :: Proxy codecTail) codecsRest j
    a <- runFn2 decoder (\k -> Object.lookup k j) lbl
    pure $ Record.insert _lbl a rec
    where
    lbl = reflectSymbol _lbl
    _lbl = (Proxy :: Proxy sym)
    (ToProp decoder) = Record.get _lbl codecs

    codecsRest :: { | cRest }
    codecsRest = unsafeCoerce codecs
else instance toRecordObjFailure ::
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'ToProp a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
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
  , Row.Cons sym (FromProp i a) cRest codecs
  , Row.Cons sym a vRest values
  ) =>
  FromRecordObj (RL.Cons sym (FromProp i a) codecTail) { | codecs } { | values } where
  fromRecordObj _ codecs values = do
    let obj = fromRecordObj (Proxy :: Proxy codecTail) cRest vRest
    case encoder a' of
      Nothing -> obj
      Just (Tuple k v) -> Object.insert (fromMaybe lbl k) v obj
    where
    lbl = reflectSymbol _lbl
    _lbl = (Proxy :: Proxy sym)
    (FromProp encoder) = Record.get _lbl codecs
    a' = Record.get _lbl values
    cRest = unsafeCoerce codecs
    vRest = unsafeCoerce values
else instance fromRecordObjFailure ::
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'FromProp a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `fromRequired*`/`fromOption*` variants.")
          )
      )
  ) =>
  FromRecordObj (RL.Cons sym a codecTail) { | codecs } { | values } where
  fromRecordObj _ _ _ = unsafeCrashWith "Impossible"
