-- @inline export encoder arity=1
module Snapshot.FromRecordInline2 where

import Codec.Json.Unidirectional.Value (fromInt, fromRecord, fromRequired)
import Data.Argonaut.Core (Json)

type Foo =
  { req :: Int
  }

encoder :: Foo -> Json
encoder = fromRecord
  { req: fromRequired fromInt
  }

test :: Foo -> Json
test j = encoder j
