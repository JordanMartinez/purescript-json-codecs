module Codec.Json.Errors.PlainDodoError where

import Prelude

import Codec.Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Codec.Json.JsonDecoder (JsonDecoder, runJsonDecoder)
import Codec.Json.Types (JsonErrorHandlers(..), JsonOffset, ctorHintMsg, fieldHintMsg, printJsonOffsetPath, subtermHintMsg, typeHintMsg)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (Either)
import Data.Function.Uncurried (mkFn2, mkFn3)
import Dodo (Doc, plainText, twoSpaces)
import Dodo as D

handlersPde :: JsonErrorHandlers (Doc Void)
handlersPde = JsonErrorHandlers
  { onTypeMismatch: mkFn3 \path exp act ->
      D.lines
        [ D.text $ printTypeMismatchErr exp act
        , docifyPath path
        ]
  , onMissingField: mkFn2 \path field ->
      D.lines
        [ D.text $ printMissingField field
        , docifyPath path
        ]
  , onMissingIndex: mkFn2 \path idx ->
      D.lines
        [ D.text $ printMissingIndex idx
        , docifyPath path
        ]
  , onUnrefinableValue: mkFn2 \path msg ->
      D.lines
        [ D.text msg
        , docifyPath path
        ]
  , onStructureError: mkFn2 \path msg ->
      D.lines
        [ D.text msg
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

docifyPath :: Array JsonOffset -> Doc Void
docifyPath path = D.space <> D.space <> D.text "at path:" <> D.space <> (D.text $ printJsonOffsetPath path)

docifyHint :: Array JsonOffset -> Doc Void -> Doc Void -> Doc Void
docifyHint path msg err =
  D.lines
    [ D.lines
        [ msg
        , docifyPath path
        ]
    , D.indent err
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
