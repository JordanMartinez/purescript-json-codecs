-- @inline export decoder arity=1
module Snapshot.ToRecordInlines where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError, toBoolean, toInt, toOption, toOptionArray, toOptionAssocArray, toOptionDefault, toOptionDefaultRename, toOptionRename, toRecord, toRequired, toRequiredRename, toStatic, toString)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type Foo =
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

decoder :: Json -> Either DecodeError Foo
decoder = toRecord
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
