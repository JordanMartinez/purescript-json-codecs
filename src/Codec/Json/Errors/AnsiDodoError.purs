module Codec.Json.Errors.AnsiDodoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Dodo (Doc, twoSpaces)
import Dodo as D
import Dodo.Ansi (Color(..), GraphicsParam, ansiGraphics, foreground)
import Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Json.JsonDecoder (JsonDecoder, runJsonDecoder)
import Json.Types (JsonErrorHandlers(..), JsonOffset, TypeHint(..), printJsonOffsetPath)

handlersAde :: JsonErrorHandlers (Doc GraphicsParam)
handlersAde = JsonErrorHandlers
  { onTypeMismatch: \path exp act ->
      D.lines
        [ foreground BrightRed $ D.text $ printTypeMismatchErr exp act
        , docifyPath path
        ]
  , onMissingField: \path field ->
      D.lines
        [ foreground BrightRed $ D.text $ printMissingField field
        , docifyPath path
        ]
  , onMissingIndex: \path idx ->
      D.lines
        [ foreground BrightRed $ D.text $ printMissingIndex idx
        , docifyPath path
        ]
  , onUnrefinableValue: \path msg ->
      D.lines
        [ foreground BrightRed $ D.text msg
        , docifyPath path
        ]
  , onStructureError: \path msg ->
      D.lines
        [ foreground BrightRed $ D.text msg
        , docifyPath path
        ]
  , includeJsonOffset: true
  , addHint: \path hint err ->
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
