-- @inline export encoder arity=1
module Snapshot.FromRecordInline1 where

import Codec.Json.Unidirectional.Value (fromRecord)
import Data.Argonaut.Core (Json)

type Foo =
  {}

encoder :: Foo -> Json
encoder = fromRecord
  {
  }

test :: Foo -> Json
test j = encoder j
