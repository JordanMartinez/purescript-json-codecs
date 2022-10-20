module Json.Errors.PlainDodoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List.Types (List, NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String (CodePoint)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Symbol (class IsSymbol)
import Data.These (These)
import Data.Tuple (Tuple)
import Dodo (Doc)
import Dodo as D
import Foreign.Object (Object)
import Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Json.Primitive.Decode (class IsDecodeJsonError, JsonDecoder, JsonOffset, TypeHint, failWithPath, onError, printJsonOffsetPath, printTypeHint)
import Json.Unidirectional.Decode.Value (class DecodeRowList, class InsertOptionalPropDecoders, class InsertRequiredPropDecoders, class RebuildRecord, PropDecoder, RLRecordDecoder, RLRecordDecoderBuilder)
import Json.Unidirectional.Decode.Value as JUDV
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Type.Proxy (Proxy)

newtype PlainDodoError = PlainDodoError (Doc Void)

derive instance Newtype PlainDodoError _

instance Semigroup PlainDodoError where
  append (PlainDodoError l) (PlainDodoError r) =
    PlainDodoError $ l <> D.break <> D.break <> r

instance IsDecodeJsonError PlainDodoError where
  onTypeMismatch path exp act = PlainDodoError $
    D.lines
      [ D.text $ printTypeMismatchErr exp act
      , docifyPath path
      ]

  onMissingField path field = PlainDodoError $
    D.lines
      [ D.text $ printMissingField field
      , docifyPath path
      ]

  onMissingIndex path idx = PlainDodoError $
    D.lines
      [ D.text $ printMissingIndex idx
      , docifyPath path
      ]

  onUnrefinableValue path msg = PlainDodoError $
    D.lines
      [ D.text msg
      , docifyPath path
      ]

  onStructureError path msg = PlainDodoError $
    D.lines
      [ D.text msg
      , docifyPath path
      ]

  withHint hint = do
    onError \path -> do
      over PlainDodoError \r ->
        D.lines
          [ docifyHint hint path
          , D.indent r
          ]

docifyPath :: Array JsonOffset -> Doc Void
docifyPath path = D.space <> D.space <> D.text "at path:" <> D.space <> (D.text $ printJsonOffsetPath path)

docifyHint :: TypeHint -> Array JsonOffset -> Doc Void
docifyHint hint path =
  D.lines
    [ D.text $ printTypeHint hint
    , docifyPath path
    ]

printPlainDodoError :: forall b r. D.Printer b Void r -> D.PrintOptions -> PlainDodoError -> r
printPlainDodoError printer options = D.print printer options <<< unwrap

decodeVoid :: JsonDecoder PlainDodoError Void
decodeVoid = JUDV.decodeVoid

decodeNullToUnit :: JsonDecoder PlainDodoError Unit
decodeNullToUnit = JUDV.decodeNullToUnit

decodeNull :: JsonDecoder PlainDodoError Unit
decodeNull = JUDV.decodeNull

decodeBoolean :: JsonDecoder PlainDodoError Boolean
decodeBoolean = JUDV.decodeBoolean

decodeNumber :: JsonDecoder PlainDodoError Number
decodeNumber = JUDV.decodeNumber

decodeString :: JsonDecoder PlainDodoError String
decodeString = JUDV.decodeString

decodeArrayPrim :: JsonDecoder PlainDodoError (Array Json)
decodeArrayPrim = JUDV.decodeArrayPrim

decodeIndex :: forall a. Array Json -> Int -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a
decodeIndex = JUDV.decodeIndex

decodeIndex' :: forall a. Array Json -> Int -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a
decodeIndex' = JUDV.decodeIndex'

decodeObjectPrim :: JsonDecoder PlainDodoError (Object Json)
decodeObjectPrim = JUDV.decodeObjectPrim

decodeField :: forall a. Object Json -> String -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a
decodeField = JUDV.decodeField

decodeField' :: forall a. Object Json -> String -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a -> JsonDecoder PlainDodoError a
decodeField' = JUDV.decodeField'

decodeInt :: JsonDecoder PlainDodoError Int
decodeInt = JUDV.decodeInt

decodeChar :: JsonDecoder PlainDodoError Char
decodeChar = JUDV.decodeChar

decodeNonEmptyString :: JsonDecoder PlainDodoError NonEmptyString
decodeNonEmptyString = JUDV.decodeNonEmptyString

decodeArray
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Array a)
decodeArray = JUDV.decodeArray

decodeNonEmptyArray
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (NonEmptyArray a)
decodeNonEmptyArray = JUDV.decodeNonEmptyArray

decodeObject
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Object a)
decodeObject = JUDV.decodeObject

decodeNullable
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Nullable a)
decodeNullable = JUDV.decodeNullable

decodeIdentity
  :: forall a
   . Coercible (JsonDecoder PlainDodoError a) (JsonDecoder PlainDodoError (Identity a))
  => JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Identity a)
decodeIdentity = JUDV.decodeIdentity

decodeMaybeTagged
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Maybe a)
decodeMaybeTagged = JUDV.decodeMaybeTagged

decodeMaybeNullable
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Maybe a)
decodeMaybeNullable = JUDV.decodeMaybeNullable

decodeEither
  :: forall a b
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError b
  -> JsonDecoder PlainDodoError (Either a b)
decodeEither = JUDV.decodeEither

decodeTuple
  :: forall a b
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError b
  -> JsonDecoder PlainDodoError (Tuple a b)
decodeTuple = JUDV.decodeTuple

decodeThese
  :: forall a b
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError b
  -> JsonDecoder PlainDodoError (These a b)
decodeThese = JUDV.decodeThese

decodeNonEmpty
  :: forall f a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (f a)
  -> JsonDecoder PlainDodoError (NonEmpty f a)
decodeNonEmpty = JUDV.decodeNonEmpty

decodeList
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (List a)
decodeList = JUDV.decodeList

decodeNonEmptyList
  :: forall a
   . JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (NonEmptyList a)
decodeNonEmptyList = JUDV.decodeNonEmptyList

decodeMap
  :: forall k v
   . Ord k
  => JsonDecoder PlainDodoError k
  -> JsonDecoder PlainDodoError v
  -> JsonDecoder PlainDodoError (Map k v)
decodeMap = JUDV.decodeMap

decodeSet
  :: forall a
   . Ord a
  => JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (Set a)
decodeSet = JUDV.decodeSet

decodeNonEmptySet
  :: forall a
   . Ord a
  => JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError (NonEmptySet a)
decodeNonEmptySet = JUDV.decodeNonEmptySet

decodeCodePoint :: JsonDecoder PlainDodoError CodePoint
decodeCodePoint = JUDV.decodeCodePoint

decodeRecord
  :: forall propsRl props decoderRows decoderRl tuples outputRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders PlainDodoError propsRl { | props } {} { | decoderRows }
  => RowList.RowToList decoderRows decoderRl
  => DecodeRowList PlainDodoError decoderRl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => { | props }
  -> JsonDecoder PlainDodoError { | outputRows }
decodeRecord = JUDV.decodeRecord
  ( \field -> failWithPath \path -> PlainDodoError $
      D.lines
        [ D.text $ printMissingField field
        , docifyPath path
        ]
  )

decodeRecord'
  :: forall rl decoderRows tuples outputRows
   . DecodeRowList PlainDodoError rl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => RLRecordDecoder PlainDodoError rl { | decoderRows }
  -> JsonDecoder PlainDodoError { | outputRows }
decodeRecord' = JUDV.decodeRecord'

buildRecordDecoder
  :: forall rl decoderRows
   . RowList.RowToList decoderRows rl
  => RLRecordDecoderBuilder PlainDodoError {} { | decoderRows }
  -> RLRecordDecoder PlainDodoError rl { | decoderRows }
buildRecordDecoder = JUDV.buildRecordDecoder

decodeRequiredProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder PlainDodoError a) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder PlainDodoError a
  -> JsonDecoder PlainDodoError a
  -> RLRecordDecoderBuilder PlainDodoError { | oldRows } { | newRows }
decodeRequiredProp = JUDV.decodeRequiredProp

decodeOptionalProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder PlainDodoError (Maybe a)) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder PlainDodoError a
  -> RLRecordDecoderBuilder PlainDodoError { | oldRows } { | newRows }
decodeOptionalProp = JUDV.decodeOptionalProp

decodeRequiredProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders PlainDodoError propsRl { | props } { | oldRows } { | newRows }
  => (forall a. String -> JsonDecoder PlainDodoError a)
  -> { | props }
  -> (RLRecordDecoderBuilder PlainDodoError { | oldRows } { | newRows })
decodeRequiredProps = JUDV.decodeRequiredProps

decodeOptionalProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropDecoders PlainDodoError propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder PlainDodoError { | oldRows } { | newRows })
decodeOptionalProps = JUDV.decodeOptionalProps
