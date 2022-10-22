module Json.Primitive.Decode.Qualified where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Json.Primitive.Decode (JsonDecoder(..))
import Data.V.Semigroup.Qualified as V

bind :: forall err extra a b. JsonDecoder err extra a -> (a -> JsonDecoder err extra b) -> JsonDecoder err extra b
bind (JsonDecoder (ReaderT ma)) f = JsonDecoder $ ReaderT \r -> V.do
  a <- ma r
  let (JsonDecoder (ReaderT mb)) = f a
  mb r
