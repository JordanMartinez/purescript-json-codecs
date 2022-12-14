module Codec.Json.Errors.AnsiDodoError where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Codes as Ansi
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

fgText :: Ansi.Color -> String -> Doc GraphicsParam
fgText c t = foreground c $ D.text t

handlersAde :: JsonErrorHandlers (Doc GraphicsParam)
handlersAde = JsonErrorHandlers
  { onTypeMismatch: mkFn3 \path exp act ->
      (fgText BrightRed $ printTypeMismatchErr exp act) <> D.space <> docifyPath BrightCyan path
  , onMissingField: mkFn2 \path field ->
      (fgText BrightRed $ printMissingField field) <> D.space <> docifyPath BrightCyan path
  , onMissingIndex: mkFn2 \path idx ->
      (fgText BrightRed $ printMissingIndex idx) <> D.space <> docifyPath BrightCyan path
  , onUnrefinableValue: mkFn2 \path msg ->
      fgText BrightRed msg <> D.space <> docifyPath BrightCyan path
  , onStructureError: mkFn2 \path msg ->
      fgText BrightRed msg <> D.space <> docifyPath BrightCyan path
  , addJsonOffset: mkFn2 \a b -> Array.snoc a b
  , addTypeHint: mkFn3 \path hint err ->
      docifyHint path (D.text typeHintMsg <> (foreground Yellow $ D.text hint)) err
  , addCtorHint: mkFn3 \path hint err ->
      docifyHint path (D.text ctorHintMsg <> (foreground Yellow $ D.text hint)) err
  , addSubtermHint: mkFn3 \path hint err ->
      docifyHint path (D.text subtermHintMsg <> (foreground Yellow $ D.text $ show hint)) err
  , addFieldHint: mkFn3 \path hint err ->
      docifyHint path (D.text fieldHintMsg <> (foreground Yellow $ D.text hint)) err
  }

docifyPath :: Ansi.Color -> Array JsonOffset -> Doc GraphicsParam
docifyPath pathColor path =
  (fgText White "at path:" <> D.space) <> (foreground pathColor $ D.text $ printJsonOffsetPath path)

docifyHint :: Array JsonOffset -> Doc GraphicsParam -> Doc GraphicsParam -> Doc GraphicsParam
docifyHint path msg err =
  D.lines
    [ foreground White msg <> fgText White ", " <> docifyPath Cyan path
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
