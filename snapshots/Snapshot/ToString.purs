-- @inline export Snapshot.ToString.decoder arity=1
module Snapshot.ToString where

import Prelude

import Codec.Json.Decoders.SpeedyDecoder (SpeedyDecoder, runSpeedyDecoder)
import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Codec.Json.Unidirectional.Value (toString)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)

decoder :: forall @f. IsJsonDecoder f => Json -> f String
decoder = toString @f

test :: Json -> Maybe String
test j = runSpeedyDecoder $ decoder @SpeedyDecoder j
