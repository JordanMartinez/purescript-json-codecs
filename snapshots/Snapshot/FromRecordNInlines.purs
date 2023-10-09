-- @inline export encoder arity=1
module Snapshot.FromRecordNInlines where

import Prelude

import Codec.Json.Unidirectional.Value (fromBoolean, fromInt, fromOption, fromOption', fromOptionArray, fromOptionArray', fromOptionAssocArray, fromOptionAssocArray', fromOptionRename, fromOptionRename', fromRecord, fromRecordN, fromRequired, fromRequired', fromRequiredRename, fromRequiredRename', fromString)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

newtype Foo = Foo
  { req :: Int
  , req' :: Int
  , reqRen :: String
  , reqRen' :: String
  , opt :: Maybe String
  , opt' :: Maybe String
  , optRen :: Maybe String
  , optRen' :: Maybe String
  , optArr :: Array String
  , optArr' :: Array String
  , optAssocArr :: Array (Tuple String String)
  , optAssocArr' :: Array (Tuple String String)
  , nested ::
      { other :: Boolean
      , foo :: Maybe Boolean
      }
  }

derive instance Newtype Foo _

encoder :: Foo -> Json
encoder = fromRecordN Foo
  { req: fromRequired fromInt
  , req': fromRequired' @1 fromInt
  , reqRen: fromRequiredRename "otherName" fromString
  , reqRen': fromRequiredRename' @2 "otherName2" fromString
  , opt: fromOption fromString
  , opt': fromOption' @3 fromString
  , optRen: fromOptionRename "otherName2" fromString
  , optRen': fromOptionRename' @4 "otherNameA" fromString
  , optArr: fromOptionArray fromString
  , optArr': fromOptionArray' @5 fromString
  , optAssocArr: fromOptionAssocArray show fromString
  , optAssocArr': fromOptionAssocArray' @6 show fromString
  , nested: fromRequired $ fromRecord
      { other: fromRequired fromBoolean
      , foo: fromOption fromBoolean
      }
  }
