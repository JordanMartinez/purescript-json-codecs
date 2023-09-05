-- @inline export Snapshot.ToString.decoder arity=1
module Snapshot.ToString where

import Codec.Json.Unidirectional.Value (toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.List (List)

decoder :: Json -> Either (List String) String
decoder = toString

test :: Json -> Either (List String) String
test j = decoder j
