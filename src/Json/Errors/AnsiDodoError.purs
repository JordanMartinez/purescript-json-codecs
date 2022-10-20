module Json.Errors.AnsiDodoError where

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
import Dodo.Ansi (Color(..), GraphicsParam, foreground)
import Foreign.Object (Object)
import Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Json.Primitive.Decode (class IsDecodeJsonError, JsonDecoder, JsonOffset, TypeHint(..), failWithPath, onError, printJsonOffsetPath)
import Json.Unidirectional.Decode.Value (class DecodeRowList, class InsertOptionalPropDecoders, class InsertRequiredPropDecoders, class RebuildRecord, PropDecoder, RLRecordDecoder, RLRecordDecoderBuilder)
import Json.Unidirectional.Decode.Value as JUDV
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RowList
import Type.Proxy (Proxy)

newtype AnsiDodoError = AnsiDodoError (Doc GraphicsParam)

derive instance Newtype AnsiDodoError _

instance Semigroup AnsiDodoError where
  append (AnsiDodoError l) (AnsiDodoError r) =
    AnsiDodoError $ l <> D.break <> D.break <> r

instance IsDecodeJsonError AnsiDodoError where
  onTypeMismatch path exp act = AnsiDodoError $
    D.lines
      [ foreground BrightRed $ D.text $ printTypeMismatchErr exp act
      , docifyPath path
      ]

  onMissingField path field = AnsiDodoError $
    D.lines
      [ foreground BrightRed $ D.text $ printMissingField field
      , docifyPath path
      ]

  onMissingIndex path idx = AnsiDodoError $
    D.lines
      [ foreground BrightRed $ D.text $ printMissingIndex idx
      , docifyPath path
      ]

  onUnrefinableValue path msg = AnsiDodoError $
    D.lines
      [ foreground BrightRed $ D.text msg
      , docifyPath path
      ]

  onStructureError path msg = AnsiDodoError $
    D.lines
      [ foreground BrightRed $ D.text msg
      , docifyPath path
      ]

  withHint hint = do
    onError \path -> do
      over AnsiDodoError \r ->
        D.lines
          [ docifyHint hint path
          , D.indent r
          ]

docifyPath :: Array JsonOffset -> Doc GraphicsParam
docifyPath path = D.space <> D.space <> D.text "at path:" <> D.space <> (foreground Cyan $ D.text $ printJsonOffsetPath path)

docifyHint :: TypeHint -> Array JsonOffset -> Doc GraphicsParam
docifyHint hint path =
  D.lines
    [ printHint hint
    , docifyPath path
    ]
  where
  printHint :: TypeHint -> Doc GraphicsParam
  printHint = case _ of
    TyName s -> D.text "while decoding the type, " <> (foreground BrightYellow $ D.text s)
    CtorName s -> D.text "while decoding the constructor, " <> (foreground BrightYellow $ D.text s)
    Subterm i -> D.text "while decoding the subterm at index, " <> (foreground BrightYellow $ D.text $ show i)
    Field f -> D.text "while decoding the value under the label, " <> (foreground BrightYellow $ D.text f)

printAnsiDodoError :: forall b r. D.Printer b GraphicsParam r -> D.PrintOptions -> AnsiDodoError -> r
printAnsiDodoError printer options = D.print printer options <<< unwrap

decodeVoid :: JsonDecoder AnsiDodoError Void
decodeVoid = JUDV.decodeVoid

decodeNullToUnit :: JsonDecoder AnsiDodoError Unit
decodeNullToUnit = JUDV.decodeNullToUnit

decodeNull :: JsonDecoder AnsiDodoError Unit
decodeNull = JUDV.decodeNull

decodeBoolean :: JsonDecoder AnsiDodoError Boolean
decodeBoolean = JUDV.decodeBoolean

decodeNumber :: JsonDecoder AnsiDodoError Number
decodeNumber = JUDV.decodeNumber

decodeString :: JsonDecoder AnsiDodoError String
decodeString = JUDV.decodeString

decodeArrayPrim :: JsonDecoder AnsiDodoError (Array Json)
decodeArrayPrim = JUDV.decodeArrayPrim

decodeIndex :: forall a. Array Json -> Int -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a
decodeIndex = JUDV.decodeIndex

decodeIndex' :: forall a. Array Json -> Int -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a
decodeIndex' = JUDV.decodeIndex'

decodeObjectPrim :: JsonDecoder AnsiDodoError (Object Json)
decodeObjectPrim = JUDV.decodeObjectPrim

decodeField :: forall a. Object Json -> String -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a
decodeField = JUDV.decodeField

decodeField' :: forall a. Object Json -> String -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a -> JsonDecoder AnsiDodoError a
decodeField' = JUDV.decodeField'

decodeInt :: JsonDecoder AnsiDodoError Int
decodeInt = JUDV.decodeInt

decodeChar :: JsonDecoder AnsiDodoError Char
decodeChar = JUDV.decodeChar

decodeNonEmptyString :: JsonDecoder AnsiDodoError NonEmptyString
decodeNonEmptyString = JUDV.decodeNonEmptyString

decodeArray
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Array a)
decodeArray = JUDV.decodeArray

decodeNonEmptyArray
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (NonEmptyArray a)
decodeNonEmptyArray = JUDV.decodeNonEmptyArray

decodeObject
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Object a)
decodeObject = JUDV.decodeObject

decodeNullable
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Nullable a)
decodeNullable = JUDV.decodeNullable

decodeIdentity
  :: forall a
   . Coercible (JsonDecoder AnsiDodoError a) (JsonDecoder AnsiDodoError (Identity a))
  => JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Identity a)
decodeIdentity = JUDV.decodeIdentity

decodeMaybeTagged
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Maybe a)
decodeMaybeTagged = JUDV.decodeMaybeTagged

decodeMaybeNullable
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Maybe a)
decodeMaybeNullable = JUDV.decodeMaybeNullable

decodeEither
  :: forall a b
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError b
  -> JsonDecoder AnsiDodoError (Either a b)
decodeEither = JUDV.decodeEither

decodeTuple
  :: forall a b
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError b
  -> JsonDecoder AnsiDodoError (Tuple a b)
decodeTuple = JUDV.decodeTuple

decodeThese
  :: forall a b
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError b
  -> JsonDecoder AnsiDodoError (These a b)
decodeThese = JUDV.decodeThese

decodeNonEmpty
  :: forall f a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (f a)
  -> JsonDecoder AnsiDodoError (NonEmpty f a)
decodeNonEmpty = JUDV.decodeNonEmpty

decodeList
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (List a)
decodeList = JUDV.decodeList

decodeNonEmptyList
  :: forall a
   . JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (NonEmptyList a)
decodeNonEmptyList = JUDV.decodeNonEmptyList

decodeMap
  :: forall k v
   . Ord k
  => JsonDecoder AnsiDodoError k
  -> JsonDecoder AnsiDodoError v
  -> JsonDecoder AnsiDodoError (Map k v)
decodeMap = JUDV.decodeMap

decodeSet
  :: forall a
   . Ord a
  => JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (Set a)
decodeSet = JUDV.decodeSet

decodeNonEmptySet
  :: forall a
   . Ord a
  => JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError (NonEmptySet a)
decodeNonEmptySet = JUDV.decodeNonEmptySet

decodeCodePoint :: JsonDecoder AnsiDodoError CodePoint
decodeCodePoint = JUDV.decodeCodePoint

decodeRecord
  :: forall propsRl props decoderRows decoderRl tuples outputRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders AnsiDodoError propsRl { | props } {} { | decoderRows }
  => RowList.RowToList decoderRows decoderRl
  => DecodeRowList AnsiDodoError decoderRl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => { | props }
  -> JsonDecoder AnsiDodoError { | outputRows }
decodeRecord = JUDV.decodeRecord
  ( \field -> failWithPath \path -> AnsiDodoError $
      D.lines
        [ D.text $ printMissingField field
        , docifyPath path
        ]
  )

decodeRecord'
  :: forall rl decoderRows tuples outputRows
   . DecodeRowList AnsiDodoError rl { | decoderRows } tuples
  => RebuildRecord tuples {} { | outputRows }
  => RLRecordDecoder AnsiDodoError rl { | decoderRows }
  -> JsonDecoder AnsiDodoError { | outputRows }
decodeRecord' = JUDV.decodeRecord'

buildRecordDecoder
  :: forall rl decoderRows
   . RowList.RowToList decoderRows rl
  => RLRecordDecoderBuilder AnsiDodoError {} { | decoderRows }
  -> RLRecordDecoder AnsiDodoError rl { | decoderRows }
buildRecordDecoder = JUDV.buildRecordDecoder

decodeRequiredProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder AnsiDodoError a) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder AnsiDodoError a
  -> JsonDecoder AnsiDodoError a
  -> RLRecordDecoderBuilder AnsiDodoError { | oldRows } { | newRows }
decodeRequiredProp = JUDV.decodeRequiredProp

decodeOptionalProp
  :: forall sym a oldRows newRows
   . Row.Cons sym (PropDecoder AnsiDodoError (Maybe a)) oldRows newRows
  => IsSymbol sym
  => Row.Lacks sym oldRows
  => Proxy sym
  -> JsonDecoder AnsiDodoError a
  -> RLRecordDecoderBuilder AnsiDodoError { | oldRows } { | newRows }
decodeOptionalProp = JUDV.decodeOptionalProp

decodeRequiredProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertRequiredPropDecoders AnsiDodoError propsRl { | props } { | oldRows } { | newRows }
  => (forall a. String -> JsonDecoder AnsiDodoError a)
  -> { | props }
  -> (RLRecordDecoderBuilder AnsiDodoError { | oldRows } { | newRows })
decodeRequiredProps = JUDV.decodeRequiredProps

decodeOptionalProps
  :: forall propsRl props oldRows newRows
   . RowList.RowToList props propsRl
  => InsertOptionalPropDecoders AnsiDodoError propsRl { | props } { | oldRows } { | newRows }
  => { | props }
  -> (RLRecordDecoderBuilder AnsiDodoError { | oldRows } { | newRows })
decodeOptionalProps = JUDV.decodeOptionalProps
