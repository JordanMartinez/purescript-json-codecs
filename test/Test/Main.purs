module Test.Main where

import Prelude

import Effect (Effect)
import Test.Codec.Json.Unidirectional.Value as Value

main :: Effect Unit
main = Value.runOutput
