-- | Defines the handlers and other utilities for when you do not want
-- | any error when a JSON decoder fails.
module Codec.Json.Decoders.PlainPrettyDecoder where

import Prelude

import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Newtype (class Newtype, unwrap)
import Dodo (Doc)
import Dodo as D

newtype PlainPrettyDecoder a = PlainPrettyDecoder (String -> Either (Doc Void) a)

derive instance Newtype (PlainPrettyDecoder a) _
instance Functor PlainPrettyDecoder where
  map f (PlainPrettyDecoder m) = PlainPrettyDecoder (map f <<< m)

instance Apply PlainPrettyDecoder where
  apply (PlainPrettyDecoder f) (PlainPrettyDecoder m) = PlainPrettyDecoder (\p -> apply (f p) (m p))

instance Applicative PlainPrettyDecoder where
  pure a = PlainPrettyDecoder \_ -> Right a

instance Bind PlainPrettyDecoder where
  bind (PlainPrettyDecoder m) f = PlainPrettyDecoder \p -> do
    a <- m p
    let (PlainPrettyDecoder m') = f a
    m' p

instance Monad PlainPrettyDecoder

instance IsJsonDecoder PlainPrettyDecoder where
  onUnrefinableValue :: forall a. String -> PlainPrettyDecoder a
  onUnrefinableValue msg = PlainPrettyDecoder $ throwLeafError msg

  onTypeMismatch :: forall a. Fn2 String String (PlainPrettyDecoder a)
  onTypeMismatch = mkFn2 \exp act -> PlainPrettyDecoder $ throwLeafError $ "Expected " <> exp <> ", but got " <> act

  onMissingField :: forall a. String -> PlainPrettyDecoder a
  onMissingField key = PlainPrettyDecoder $ throwLeafError $ "Required key " <> show key <> " was missing."

  onMissingIndex :: forall a. Int -> PlainPrettyDecoder a
  onMissingIndex idx = PlainPrettyDecoder $ throwLeafError $ "Required index " <> show idx <> " was missing."

  onStructureError :: forall a. String -> PlainPrettyDecoder a
  onStructureError msg = PlainPrettyDecoder $ throwLeafError msg

  atKey :: forall a. String -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  atKey key (PlainPrettyDecoder m) = PlainPrettyDecoder $ \path -> m $ path <> (".\"" <> key <> "\"")

  atIndex :: forall a. Int -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  atIndex idx (PlainPrettyDecoder m) = PlainPrettyDecoder $ \path -> m $ path <> ("[" <> show idx <> "]")

  addTypeHint :: forall a. String -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  addTypeHint ty (PlainPrettyDecoder m) = PlainPrettyDecoder $ addHint ("while decoding type, " <> ty) m

  addCtorHint :: forall a. String -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  addCtorHint ctor (PlainPrettyDecoder m) = PlainPrettyDecoder $ addHint ("while decoding constructor, " <> ctor) m

  addSubtermHint :: forall a. Int -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  addSubtermHint idx (PlainPrettyDecoder m) = PlainPrettyDecoder $ addHint ("while decoding subterm, " <> show idx) m

  addFieldHint :: forall a. String -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  addFieldHint field (PlainPrettyDecoder m) = PlainPrettyDecoder $ addHint ("while decoding field, " <> field) m

  altAccumulate :: forall a. PlainPrettyDecoder a -> PlainPrettyDecoder a -> PlainPrettyDecoder a
  altAccumulate (PlainPrettyDecoder l) (PlainPrettyDecoder r) = PlainPrettyDecoder \path ->
    case l path of
      x@(Right _) -> x
      Left err -> case r path of
        x@(Right _) -> x
        Left err2 -> Left $
          D.lines
            [ err
            , err2
            ]

  altAccumulateLazy :: forall j a. (j -> PlainPrettyDecoder a) -> (j -> PlainPrettyDecoder a) -> j -> PlainPrettyDecoder a
  altAccumulateLazy l r j = PlainPrettyDecoder \path -> do
    case (unwrap $ l j) path of
      x@(Right _) -> x
      Left err -> do
        case (unwrap $ r j) path of
          x@(Right _) -> x
          Left err2 -> Left $
            D.lines
              [ err
              , err2
              ]

runPlainPrettyDecoder :: forall a. PlainPrettyDecoder a -> Either (Doc Void) a
runPlainPrettyDecoder (PlainPrettyDecoder f) = f "ROOT"

throwLeafError :: forall a. String -> (String -> Either (Doc Void) a)
throwLeafError msg = \path ->
  Left $ D.text $ msg <> ", at path " <> path

addHint :: forall a. String -> (String -> Either (Doc Void) a) -> (String -> Either (Doc Void) a)
addHint hint toResult = \path ->
  case toResult path of
    x@(Right _) -> x
    Left err -> Left $
      D.lines
        [ D.text $ hint <> ", at path " <> path
        , D.indent err
        ]
