-- @inline export decoder arity=1
module Snapshot.ToRecordInlines1 where

import Codec.Json.Unidirectional.Value (DecodeError, toRecord)
import Data.Argonaut.Core (Json)
import Data.Either (Either)

type Foo = {}

decoder :: Json -> Either DecodeError {}
decoder = toRecord {}

test :: Json -> Either DecodeError Foo
test j = decoder j
