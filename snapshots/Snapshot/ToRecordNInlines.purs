-- @inline export decoder arity=1
module Snapshot.ToRecordNInlines where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError, toBoolean, toInt, toOption, toOptionArray, toOptionAssocArray, toOptionDefault, toOptionDefaultRename, toOptionRename, toRecord, toRecordN, toRequired, toRequiredRename, toStatic, toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

newtype Foo = Foo
  { static :: Int
  , req :: Int
  , reqRen :: String
  , opt :: Maybe String
  , optRen :: Maybe String
  , optDef :: Int
  , optDefRen :: Int
  , optArr :: Array String
  , optAssocArr :: Array (Tuple String String)
  , nested ::
      { other :: Boolean
      , foo :: Maybe Boolean
      }
  }

derive instance Newtype Foo _

decoder :: Json -> Either DecodeError Foo
decoder = toRecordN Foo
  { static: toStatic 1
  , req: toRequired toInt
  , reqRen: toRequiredRename "otherName" toString
  , opt: toOption toString
  , optRen: toOptionRename "otherName2" toString
  , optDef: toOptionDefault 1 toInt
  , optDefRen: toOptionDefaultRename "otherName3" 2 toInt
  , optArr: toOptionArray toString
  , optAssocArr: toOptionAssocArray pure toString
  , nested: toRequired $ toRecord
      { other: toRequired toBoolean
      , foo: toOption toBoolean
      }
  }

test :: Json -> Either DecodeError Foo
test j = decoder j
