module Json.Primitive.Decode.Qualified where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Json.Primitive.Decode (JsonDecoder(..))
import Data.V.Semigroup.Qualified as V

bind :: forall err a b. JsonDecoder err a -> (a -> JsonDecoder err b) -> JsonDecoder err b
bind (JsonDecoder (ReaderT ma)) f = JsonDecoder $ ReaderT \r -> V.do
  a <- ma r
  let (JsonDecoder (ReaderT mb)) = f a
  mb r
