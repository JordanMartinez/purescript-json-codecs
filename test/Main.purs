module Test.Main where

import Prelude

import Data.Traversable (sequence_)
import Effect (Effect)
import Test.Json.Unidirectional.AnsiDodoError as ADE
import Test.Json.Unidirectional.PlainDodoError as PDE
import Test.Json.Unidirectional.PrimitiveJsonError as PJE
import Test.Json.Unidirectional.TypeClass as TypeClass

main :: Effect Unit
main = sequence_
  [ PJE.runOutput
  , PDE.runOutput
  , ADE.runOutput
  , TypeClass.runOutput
  ]
