module Codec.Json.Unidirectional.Encode.Value
  ( fromVoid
  , fromJNull
  , fromNullNothingOrJust
  , fromUnitToNull
  , fromBoolean
  , fromNumber
  , fromInt
  , fromChar
  , fromString
  , fromNonEmptyString
  , fromArray
  , fromJArray
  , fromNonEmptyArray
  , fromObject
  , fromJObject
  , fromNullable
  , fromIdentity
  , fromMaybeTagged
  , fromMaybeNullable
  , fromEither
  , fromTuple
  , fromThese
  , fromNonEmpty
  , fromList
  , fromNonEmptyList
  , fromMap
  , fromSet
  , fromNonEmptySet
  , fromCodePoint
  , fromRecord
  , fromRecordN
  , FromRecordCodec(..)
  , fromRequired
  , fromRequiredRename
  , fromOption
  , fromOptionRename
  , fromOptionArray
  , fromOptionAssocArray
  , class FromRecordObj
  , fromRecordObj
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet, toSet)
import Data.String (CodePoint)
import Data.String as SCP
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Symbol (class IsSymbol)
import Data.These (These, these)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

fromVoid :: Void -> Json
fromVoid = absurd

fromJNull :: Json
fromJNull = Json.jsonNull

fromNullNothingOrJust :: forall a. (a -> Json) -> Maybe a -> Json
fromNullNothingOrJust f = maybe Json.jsonNull f

fromUnitToNull :: Unit -> Json
fromUnitToNull = const fromJNull

fromBoolean :: Boolean -> Json
fromBoolean = Json.fromBoolean

fromNumber :: Number -> Json
fromNumber = Json.fromNumber

fromString :: String -> Json
fromString = Json.fromString

fromInt :: Int -> Json
fromInt = toNumber >>> fromNumber

fromChar :: Char -> Json
fromChar = SCU.singleton >>> fromString

fromNonEmptyString :: NonEmptyString -> Json
fromNonEmptyString (NonEmptyString s) = fromString s

fromJArray :: Array Json -> Json
fromJArray = Json.fromArray

fromArray :: forall a. (a -> Json) -> Array a -> Json
fromArray fromA = map fromA >>> fromJArray

fromNonEmptyArray :: forall a. (a -> Json) -> NonEmptyArray a -> Json
fromNonEmptyArray fromA = NEA.toArray >>> fromArray fromA

fromJObject :: Object Json -> Json
fromJObject = Json.fromObject

fromObject :: forall a. (a -> Json) -> Object a -> Json
fromObject fromA = map fromA >>> fromJObject

fromNullable :: forall a. (a -> Json) -> Nullable a -> Json
fromNullable fromA = toMaybe >>> fromMaybeNullable fromA

fromIdentity :: forall a. (a -> Json) -> Identity a -> Json
fromIdentity fromA (Identity a) = fromA a

fromMaybeTagged :: forall a. (a -> Json) -> Maybe a -> Json
fromMaybeTagged fromA =
  maybe (Object.singleton "tag" $ fromString "Nothing") (tagged "Just" <<< fromA)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

fromMaybeNullable :: forall a. (a -> Json) -> Maybe a -> Json
fromMaybeNullable fromA = maybe fromJNull fromA

fromEither :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
fromEither fromA fromB =
  either (tagged "Left" <<< fromA) (tagged "Right" <<< fromB)
    >>> fromJObject
  where
  tagged tag j = Object.fromFoldable [ Tuple "tag" $ fromString tag, Tuple "value" j ]

fromTuple :: forall a b. (a -> Json) -> (b -> Json) -> Tuple a b -> Json
fromTuple fromA fromB (Tuple a b) =
  fromJArray
    [ fromA a
    , fromB b
    ]

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

fromNonEmpty :: forall f a. (a -> Json) -> (f a -> Json) -> NonEmpty f a -> Json
fromNonEmpty fromHead fromTail (NonEmpty a fa) =
  fromJObject
    $ Object.fromFoldable
        [ Tuple "head" $ fromHead a
        , Tuple "tail" $ fromTail fa
        ]

fromList :: forall a. (a -> Json) -> List a -> Json
fromList fromA = foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

fromNonEmptyList :: forall a. (a -> Json) -> (NonEmptyList a -> Json)
fromNonEmptyList fromA = foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

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

fromSet :: forall a. (a -> Json) -> Set a -> Json
fromSet fromA = foldl (\arr -> Array.snoc arr <<< fromA) [] >>> fromJArray

fromNonEmptySet :: forall a. (a -> Json) -> NonEmptySet a -> Json
fromNonEmptySet fromA = toSet >>> fromSet fromA

fromCodePoint :: CodePoint -> Json
fromCodePoint = SCP.singleton >>> fromString

newtype FromRecordCodec a = FromRecordCodec (Tuple (Maybe String) (String -> a -> Maybe Json))

fromRequired :: forall a. (a -> Json) -> FromRecordCodec a
fromRequired f = FromRecordCodec $ Tuple Nothing \_ -> Just <<< f

fromRequiredRename :: forall a. String -> (a -> Json) -> FromRecordCodec a
fromRequiredRename str f = FromRecordCodec $ Tuple (Just str) \_ -> Just <<< f

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> Json) -> FromRecordCodec (Maybe a)
fromOption f = FromRecordCodec $ Tuple Nothing \_ -> map f

fromOptionRename :: forall a. String -> (a -> Json) -> FromRecordCodec (Maybe a)
fromOptionRename str f = FromRecordCodec $ Tuple (Just str) \_ -> map f

fromOptionArray :: forall a. (a -> Json) -> FromRecordCodec (Array a)
fromOptionArray f = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ fromArray f arr

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> Json) -> FromRecordCodec (Array (Tuple a b))
fromOptionAssocArray k' v' = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ Json.fromObject $ Array.foldl (\acc (Tuple k v) -> Object.insert (k' k) (v' v) acc) Object.empty arr

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

class FromRecordObj :: RowList Type -> Type -> Type -> Constraint
class FromRecordObj codecsRL codecs values | codecsRL -> codecs values where
  fromRecordObj :: Proxy codecsRL -> codecs -> values -> Object Json

instance FromRecordObj RL.Nil {} {} where
  fromRecordObj _ _ _ = Object.empty

instance
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
