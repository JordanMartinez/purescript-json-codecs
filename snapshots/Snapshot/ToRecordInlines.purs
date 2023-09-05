-- @inline export Snapshot.ToRecordInlines.decoder arity=1
module Snapshot.ToRecordInlines where

import Prelude

import Codec.Json.Unidirectional.Value (toBoolean, toInt, toOption, toOptionArray, toOptionRename, toRecord, toRequired, toRequiredRename, toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.List (List)
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

decoder :: Json -> Either (List String) Foo
decoder = toRecord
  { req: toRequired toInt
  , reqRen: toRequiredRename "otherName" toString
  , opt: toOption toString
  , optRen: toOptionRename "otherName2" toString
  , optArr: toOptionArray toString
  , nested: toRequired $ toRecord
      { other: toRequired toBoolean
      , foo: toOption toBoolean
      }
  }

test :: Json -> Either (List String) Foo
test j = decoder j
