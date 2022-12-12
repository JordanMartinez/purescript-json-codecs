module Codec.Json.Errors.AnsiDodoError where

import Prelude

import Codec.Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Codec.Json.JsonDecoder (JsonDecoder, runJsonDecoder)
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset, TypeHint(..), printJsonOffsetPath)
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
  , addHint: mkFn3 \path hint err ->
      D.lines
        [ docifyHint hint path
        , D.indent err
        ]
  }

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

printAnsiDodoError :: Doc GraphicsParam -> String
printAnsiDodoError = D.print ansiGraphics twoSpaces

runJsonDecoderADE :: forall a. Json -> JsonDecoder (Doc GraphicsParam) Unit a -> Either (Doc GraphicsParam) a
runJsonDecoderADE = runJsonDecoderADE' unit

runJsonDecoderADE' :: forall a extra. extra -> Json -> JsonDecoder (Doc GraphicsParam) extra a -> Either (Doc GraphicsParam) a
runJsonDecoderADE' = runJsonDecoder handlersAde (\l r -> l <> D.break <> D.break <> r)

ade :: forall a. JsonDecoder (Doc GraphicsParam) Unit a -> JsonDecoder (Doc GraphicsParam) Unit a
ade = identity

ade' :: forall a extra. JsonDecoder (Doc GraphicsParam) extra a -> JsonDecoder (Doc GraphicsParam) extra a
ade' = identity
