-- | Defines the handlers and other utilities for when you want
-- | a `Doc`-based error without colors.
module Codec.Json.Errors.PlainDodoError where

import Prelude

import Codec.Json.Errors.PrimitiveJsonError (printMissingField, printMissingIndex, printTypeMismatchErr)
import Codec.Json.JsonCodec (JsonCodec, JsonCodec', decode)
import Codec.Json.JsonDecoder (JsonDecoder, JsonDecoder', runJsonDecoder)
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
      (D.text $ printTypeMismatchErr exp act) <> D.space <> docifyPath path
  , onMissingField: mkFn2 \path field ->
      (D.text $ printMissingField field) <> D.space <> docifyPath path
  , onMissingIndex: mkFn2 \path idx ->
      (D.text $ printMissingIndex idx) <> D.space <> docifyPath path
  , onUnrefinableValue: mkFn2 \path msg ->
      D.text msg <> D.space <> docifyPath path
  , onStructureError: mkFn2 \path msg ->
      D.text msg <> D.space <> docifyPath path
  , addJsonOffset: mkFn2 \a b -> Array.snoc a b
  , addTypeHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ typeHintMsg <> hint) err
  , addCtorHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ ctorHintMsg <> hint) err
  , addSubtermHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ subtermHintMsg <> show hint) err
  , addFieldHint: mkFn3 \path hint err ->
      docifyHint path (D.text $ fieldHintMsg <> hint) err
  }

docifyPath :: Array JsonOffset -> Doc Void
docifyPath path = D.text "at path:" <> D.space <> (D.text $ printJsonOffsetPath path)

docifyHint :: Array JsonOffset -> Doc Void -> Doc Void -> Doc Void
docifyHint path msg err =
  D.lines
    [ msg <> D.text ", " <> docifyPath path
    , D.indent err
    ]

printPlainDodoError :: Doc Void -> String
printPlainDodoError = D.print plainText twoSpaces

runJsonDecoderPDE :: forall a. Json -> JsonDecoder (Doc Void) Unit a -> Either (Doc Void) a
runJsonDecoderPDE = runJsonDecoderPDE' unit

runJsonDecoderPDE' :: forall a extra. extra -> Json -> JsonDecoder (Doc Void) extra a -> Either (Doc Void) a
runJsonDecoderPDE' = runJsonDecoder handlersPde (\l r -> l <> D.break <> D.break <> r)

decodeAde :: forall a. Json -> JsonCodec (Doc Void) Unit a -> Either (Doc Void) a
decodeAde = decodeAde' unit

decodeAde' :: forall a extra. extra -> Json -> JsonCodec (Doc Void) extra a -> Either (Doc Void) a
decodeAde' = decode handlersPde (\l r -> l <> D.break <> D.break <> r)

pdeD :: forall from to. JsonDecoder' (Doc Void) Unit from to -> JsonDecoder' (Doc Void) Unit from to
pdeD = identity

pdeC :: forall from to. JsonCodec' (Doc Void) Unit from to -> JsonCodec' (Doc Void) Unit from to
pdeC = identity
