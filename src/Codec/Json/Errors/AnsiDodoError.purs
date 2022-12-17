-- | Defines the handlers and other utilities for when you want
-- | a `Doc`-based error with colors.
module Codec.Json.Errors.AnsiDodoError where

import Prelude

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

-- | Defines what color to use with each part of the error message.
-- | Given an error message below, the number below each character
-- | indicates the color used for that text when rendering an
-- | `AnsiDodoError` using `handlersAde'`.:
-- | ```
-- | while decoding the type, Type, at path: ROOT.foo
-- | 444444444444444444444444455554444444444466666666
-- |
-- |   received empty string at path: ROOT.foo
-- |   111111111111111111111222222222233333333
-- | 
-- | Colors in parenthesis after label indicate the color
-- | found in `defaultAnsiDodoErrorColors`.
-- | 1 = errorMessage (BrightRed)
-- | 2 = errorText (White)
-- | 3 = errorPath (BrightCyan)
-- | 4 = hintText (White)
-- | 5 = hintType/hintCtor/hintSubterm/hintField (Yellow)
-- | 6 = hintPath (Cyan)
-- | ````
type AnsiDodoErrorColors =
  { errorMessage :: Ansi.Color
  , errorText :: Ansi.Color
  , errorPath :: Ansi.Color
  , hintText :: Ansi.Color
  , hintType :: Ansi.Color
  , hintCtor :: Ansi.Color
  , hintSubterm :: Ansi.Color
  , hintField :: Ansi.Color
  , hintPath :: Ansi.Color
  }

-- | See `AnsiDodoErrorColors`' docs
defaultAnsiDodoErrorColors :: AnsiDodoErrorColors
defaultAnsiDodoErrorColors =
  { errorMessage: BrightRed
  , errorText: White
  , errorPath: BrightCyan
  , hintText: White
  , hintType: Yellow
  , hintCtor: Yellow
  , hintSubterm: Yellow
  , hintField: Yellow
  , hintPath: Cyan
  }

handlersAde :: JsonErrorHandlers (Doc GraphicsParam)
handlersAde = handlersAde' defaultAnsiDodoErrorColors

handlersAde' :: AnsiDodoErrorColors -> JsonErrorHandlers (Doc GraphicsParam)
handlersAde' colors = JsonErrorHandlers
  { onTypeMismatch: mkFn3 \path exp act ->
      (fgText colors.errorMessage $ printTypeMismatchErr exp act) <> D.space <> docifyPath colors.errorText colors.errorPath path
  , onMissingField: mkFn2 \path field ->
      (fgText colors.errorMessage $ printMissingField field) <> D.space <> docifyPath colors.errorText colors.errorPath path
  , onMissingIndex: mkFn2 \path idx ->
      (fgText colors.errorMessage $ printMissingIndex idx) <> D.space <> docifyPath colors.errorText colors.errorPath path
  , onUnrefinableValue: mkFn2 \path msg ->
      fgText colors.errorMessage msg <> D.space <> docifyPath colors.errorText colors.errorPath path
  , onStructureError: mkFn2 \path msg ->
      fgText colors.errorMessage msg <> D.space <> docifyPath colors.errorText colors.errorPath path
  , addJsonOffset: mkFn2 \a b -> Array.snoc a b
  , addTypeHint: mkFn3 \path hint err ->
      docifyHint colors.hintText colors.hintPath path (D.text typeHintMsg <> (fgText colors.hintType hint)) err
  , addCtorHint: mkFn3 \path hint err ->
      docifyHint colors.hintText colors.hintPath path (D.text ctorHintMsg <> (fgText colors.hintCtor hint)) err
  , addSubtermHint: mkFn3 \path hint err ->
      docifyHint colors.hintText colors.hintPath path (D.text subtermHintMsg <> (fgText colors.hintSubterm $ show hint)) err
  , addFieldHint: mkFn3 \path hint err ->
      docifyHint colors.hintText colors.hintPath path (D.text fieldHintMsg <> (fgText colors.hintField hint)) err
  }

fgText :: Ansi.Color -> String -> Doc GraphicsParam
fgText c t = foreground c $ D.text t

docifyPath :: Ansi.Color -> Ansi.Color -> Array JsonOffset -> Doc GraphicsParam
docifyPath prefixColor pathColor path =
  (fgText prefixColor "at path:" <> D.space) <> (foreground pathColor $ D.text $ printJsonOffsetPath path)

docifyHint :: Ansi.Color -> Ansi.Color -> Array JsonOffset -> Doc GraphicsParam -> Doc GraphicsParam -> Doc GraphicsParam
docifyHint textColor pathColor path msg err =
  D.lines
    [ foreground textColor msg <> fgText textColor ", " <> docifyPath textColor pathColor path
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
