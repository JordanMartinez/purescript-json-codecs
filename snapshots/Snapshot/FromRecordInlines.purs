-- @inline export encoder arity=1
module Snapshot.FromRecordInlines where

import Prelude

import Codec.Json.Unidirectional.Value (fromBoolean, fromInt, fromOption, fromOptionArray, fromOptionRename, fromRecord, fromRequired, fromRequired', fromRequiredRename, fromString)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)

type Foo =
  { req :: Int
  , reqRen :: String
  , zAppearsFirst :: String
  , opt :: Maybe String
  , optRen :: Maybe String
  , optArr :: Array String
  , nested ::
      { other :: Boolean
      , foo :: Maybe Boolean
      }
  }

encoder :: Foo -> Json
encoder = fromRecord
  { req: fromRequired fromInt
  , reqRen: fromRequiredRename "otherName" fromString
  , zAppearsFirst: fromRequired' @1 fromString
  , opt: fromOption fromString
  , optRen: fromOptionRename "otherName2" fromString
  , optArr: fromOptionArray fromString
  , nested: fromRequired $ fromRecord
      { other: fromRequired fromBoolean
      , foo: fromOption fromBoolean
      }
  }

test :: Foo -> Json
test j = encoder j
