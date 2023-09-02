module Test.Main where

import Prelude

import Codec.Json.Decoders.SpeedyDecoder (SpeedyDecoder)
import Data.Traversable (sequence_)
import Effect (Effect)
import Test.Codec.Json.Unidirectional.Value as Value

main :: Effect Unit
main = 
  sequence_
    [ Value.runOutput @SpeedyDecoder show
    ]
