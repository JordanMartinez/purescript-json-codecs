-- @inline export Snapshot.ToRecordInlines.decoder arity=1
module Snapshot.ToRecordInlines where

import Prelude

import Codec.Json.Decoders.SpeedyDecoder (SpeedyDecoder, runSpeedyDecoder)
import Codec.Json.IsJsonDecoder (class IsJsonDecoder)
import Codec.Json.Unidirectional.Value (toBoolean, toInt, toOption, toOptionArray, toOptionRename, toRecord, toRequired, toRequiredRename, toString)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)

type Foo =
  { req :: Int
  , reqRen :: String
  , opt :: Maybe String
  , optRen :: Maybe String
  , optArr :: Array String
  , nested :: 
      { other :: Boolean
      , foo :: Maybe Boolean
      }
  }

decoder :: forall @f. IsJsonDecoder f => Json -> f Foo
decoder = toRecord @f
  { req: toRequired @f toInt
  , reqRen: toRequiredRename @f "otherName" toString
  , opt: toOption @f toString
  , optRen: toOptionRename @f "otherName2" toString
  , optArr: toOptionArray @f toString
  , nested: toRequired @f $ toRecord @f
      { other: toRequired @f toBoolean
      , foo: toOption @f toBoolean
      }
  }

test :: Json -> Maybe Foo
test j = runSpeedyDecoder $ decoder @SpeedyDecoder j
