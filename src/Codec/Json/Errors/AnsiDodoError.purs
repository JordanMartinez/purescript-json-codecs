module Codec.Json.Errors.AnsiDodoError where

import Prelude

import Codec.Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Codec.Json.JsonCodec (JsonCodec', JsonCodec, decode)
import Codec.Json.JsonDecoder (JsonDecoder, JsonDecoder', runJsonDecoder)
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset, ctorHintMsg, fieldHintMsg, printJsonOffsetPath, subtermHintMsg, typeHintMsg)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (Either)
import Data.Function.Uncurried (mkFn2, mkFn3)
import Dodo (Doc, twoSpaces)
import Dodo as D
import Dodo.Ansi (Color(..), GraphicsParam, ansiGraphics, foreground)

handlersAde :: JsonErrorHandlers (Doc GraphicsParam)
handlersAde = JsonErrorHandlers
  { onTypeMismatch: mkFn3 \path exp act ->
      D.lines
        [ foreground BrightRed $ D.text $ printTypeMismatchErr exp act
        , docifyPath path
        ]
  , onMissingField: mkFn2 \path field ->
      D.lines
        [ foreground BrightRed $ D.text $ printMissingField field
        , docifyPath path
        ]
  , onMissingIndex: mkFn2 \path idx ->
      D.lines
        [ foreground BrightRed $ D.text $ printMissingIndex idx
        , docifyPath path
        ]
  , onUnrefinableValue: mkFn2 \path msg ->
      D.lines
        [ foreground BrightRed $ D.text msg
        , docifyPath path
        ]
  , onStructureError: mkFn2 \path msg ->
      D.lines
        [ foreground BrightRed $ D.text msg
        , docifyPath path
        ]
  , addJsonOffset: mkFn2 \a b -> Array.snoc a b
  , addTypeHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ typeHintMsg hint) err
  , addCtorHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ ctorHintMsg hint) err
  , addSubtermHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ subtermHintMsg hint) err
  , addFieldHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ fieldHintMsg hint) err
  }

docifyPath :: Array JsonOffset -> Doc GraphicsParam
docifyPath path = D.space <> D.space <> D.text "at path:" <> D.space <> (foreground Cyan $ D.text $ printJsonOffsetPath path)

docifyHint :: Array JsonOffset -> Doc GraphicsParam -> Doc GraphicsParam -> Doc GraphicsParam
docifyHint path msg err =
  D.lines
    [ D.lines
        [ msg
        , docifyPath path
        ]
    , D.indent err
    ]

printAnsiDodoError :: Doc GraphicsParam -> String
printAnsiDodoError = D.print ansiGraphics twoSpaces

runJsonDecoderADE :: forall a. Json -> JsonDecoder (Doc GraphicsParam) Unit a -> Either (Doc GraphicsParam) a
runJsonDecoderADE = runJsonDecoderADE' unit

runJsonDecoderADE' :: forall a extra. extra -> Json -> JsonDecoder (Doc GraphicsParam) extra a -> Either (Doc GraphicsParam) a
runJsonDecoderADE' = runJsonDecoder handlersAde (\l r -> l <> D.break <> D.break <> r)

decodeAde :: forall a. Json -> JsonCodec (Doc GraphicsParam) Unit a -> Either (Doc GraphicsParam) a
decodeAde = decodeAde' unit

decodeAde' :: forall a extra. extra -> Json -> JsonCodec (Doc GraphicsParam) extra a -> Either (Doc GraphicsParam) a
decodeAde' = decode handlersAde (\l r -> l <> D.break <> D.break <> r)

adeD :: forall from to. JsonDecoder' (Doc GraphicsParam) Unit from to -> JsonDecoder' (Doc GraphicsParam) Unit from to
adeD = identity

adeC :: forall from to. JsonCodec' (Doc GraphicsParam) Unit from to -> JsonCodec' (Doc GraphicsParam) Unit from to
adeC = identity
