module Test.Main where

import Prelude

import Codec.Json.Decoders.PlainPrettyDecoder (PlainPrettyDecoder, runPlainPrettyDecoder)
import Codec.Json.Decoders.SpeedyDecoder (SpeedyDecoder, runSpeedyDecoder)
import Data.Either (either)
import Data.Traversable (sequence_)
import Dodo as D
import Effect (Effect)
import Effect.Class.Console as Console
import Test.Codec.Json.Decoders.SpeedyDecoder as SpeedyDecoder
import Test.Codec.Json.Unidirectional.Value as Value

main :: Effect Unit
main =
  sequence_
    [ Value.runOutput @SpeedyDecoder (runSpeedyDecoder >>> show)
    , Console.log "\n\n"
    , Value.runOutput @PlainPrettyDecoder (runPlainPrettyDecoder >>> either (D.print D.plainText D.twoSpaces) show)
    , Console.log "\n\n"
    , SpeedyDecoder.runTest
    ]
