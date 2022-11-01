module Json.Errors.PlainDodoError where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Dodo (Doc, plainText, twoSpaces)
import Dodo as D
import Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Json.JsonDecoder (JsonDecoder, JsonErrorHandlers(..), JsonOffset, TypeHint, printJsonOffsetPath, printTypeHint, runJsonDecoder)

handlersPde :: JsonErrorHandlers (Doc Void)
handlersPde = JsonErrorHandlers
  { onTypeMismatch: \path exp act ->
      D.lines
        [ D.text $ printTypeMismatchErr exp act
        , docifyPath path
        ]
  , onMissingField: \path field ->
      D.lines
        [ D.text $ printMissingField field
        , docifyPath path
        ]
  , onMissingIndex: \path idx ->
      D.lines
        [ D.text $ printMissingIndex idx
        , docifyPath path
        ]
  , onUnrefinableValue: \path msg ->
      D.lines
        [ D.text msg
        , docifyPath path
        ]
  , onStructureError: \path msg ->
      D.lines
        [ D.text msg
        , docifyPath path
        ]
  , includeJsonOffset: true
  , addHint: \path hint err ->
      D.lines
        [ docifyHint hint path
        , D.indent err
        ]
  }

docifyPath :: Array JsonOffset -> Doc Void
docifyPath path = D.space <> D.space <> D.text "at path:" <> D.space <> (D.text $ printJsonOffsetPath path)

docifyHint :: TypeHint -> Array JsonOffset -> Doc Void
docifyHint hint path =
  D.lines
    [ D.text $ printTypeHint hint
    , docifyPath path
    ]

printPlainDodoError :: Doc Void -> String
printPlainDodoError = D.print plainText twoSpaces

runJsonDecoderPDE :: forall a. Json -> JsonDecoder (Doc Void) Unit a -> Either (Doc Void) a
runJsonDecoderPDE = runJsonDecoderPDE' unit

runJsonDecoderPDE' :: forall a extra. extra -> Json -> JsonDecoder (Doc Void) extra a -> Either (Doc Void) a
runJsonDecoderPDE' = runJsonDecoder handlersPde (\l r -> l <> D.break <> D.break <> r)

pde :: forall a. JsonDecoder (Doc Void) Unit a -> JsonDecoder (Doc Void) Unit a
pde = identity

pde' :: forall a extra. JsonDecoder (Doc Void) extra a -> JsonDecoder (Doc Void) extra a
pde' = identity
