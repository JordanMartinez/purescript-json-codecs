-- @inline export Snapshot.ToString.decoder arity=1
module Snapshot.ToString where

import Codec.Json.Unidirectional.Value (DecodeError, toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)

decoder :: Json -> Either DecodeError String
decoder = toString

test :: Json -> Either DecodeError String
test j = decoder j
