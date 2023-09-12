-- @inline export decoder arity=1
module Snapshot.ToRecordInlines2 where

import Codec.Json.Unidirectional.Value (DecodeError, toInt, toRecord, toRequired)
import Data.Argonaut.Core (Json)
import Data.Either (Either)

type Foo =
  { req :: Int
  }

encoder :: Json -> Either DecodeError Foo
encoder = toRecord
  { req: toRequired toInt
  }

test :: Json -> Either DecodeError Foo
test j = encoder j
