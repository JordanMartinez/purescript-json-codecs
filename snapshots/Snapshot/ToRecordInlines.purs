-- @inline export decoder arity=1
module Snapshot.ToRecordInlines where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError, toBoolean, toInt, toOption, toOptionArray, toOptionRename, toRecord, toRequired, toRequiredRename, toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
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

decoder :: Json -> Either DecodeError Foo
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

test :: Json -> Either DecodeError Foo
test j = decoder j
