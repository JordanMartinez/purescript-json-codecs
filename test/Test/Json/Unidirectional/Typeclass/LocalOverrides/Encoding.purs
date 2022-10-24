module Test.Json.Unidirectional.Typeclass.LocalOverrides.Encoding where

import Prelude

import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), swap)
import Effect (Effect)
import Effect.Class.Console (log)
import Json.Encode.Class (ExistentialEncoder0, ExistentialEncoder2, encodeJson, encodeJson', mkExistentialEncoder0, mkExistentialEncoder2)
import Json.Primitive.Encode (encodeString)
import Json.Types (K0(..), K2(..))
import Json.Unidirectional.Encode.Value (encodeTuple)
import Safe.Coerce (coerce)

runOutput :: Effect Unit
runOutput = do
  log "\n### Typeclass - Local Overrides - Encoding Output:"
  log "Starting record: "
  log $ prettyPrintRecord recordValue
  let jsonValue = encodeJson recordValue
  log "Normal encoded Json: "
  log $ stringifyWithIndent 2 jsonValue
  log "Locally-overridden encoded Json: "
  log $ stringifyWithIndent 2 $ encodeJson' localEncodeOverrides $ fromExampleRecordType recordValue
  log "==="

type ExampleRecordType =
  { stringNormal :: String
  , stringOverride :: String
  , tupleNormal :: Tuple String String
  , tupleSwap :: Tuple String String
  , tupleMultiOverride :: Tuple String String
  }

recordValue :: ExampleRecordType
recordValue =
  { stringNormal: "hello"
  , stringOverride: "+"
  , tupleNormal: Tuple "left" "right"
  , tupleSwap: Tuple "left" "right"
  , tupleMultiOverride: Tuple "left" "+"
  }

prettyPrintRecord :: ExampleRecordType -> String
prettyPrintRecord r = Array.intercalate "\n"
  [ "{ stringNormal: " <> show r.stringNormal
  , ", stringOverride: " <> show r.stringOverride
  , ", tupleNormal: " <> show r.tupleNormal
  , ", tupleSwap: " <> show r.tupleSwap
  , ", tupleMultiOverride: " <> show r.tupleMultiOverride
  , "}"
  ]

newtype LocalEncodeOverrides = LocalEncodeOverrides
  { wrapInMathExpr :: ExistentialEncoder0 String
  , swapDecoders :: ExistentialEncoder2 Tuple
  }

derive instance Newtype LocalEncodeOverrides _

localEncodeOverrides :: LocalEncodeOverrides
localEncodeOverrides = LocalEncodeOverrides
  { wrapInMathExpr: mkExistentialEncoder0 $ mkFn2 \_ plus -> encodeString $ "1 " <> plus <> " 1 == 2"
  , swapDecoders: mkExistentialEncoder2 $ \encodeA encodeB ->
      mkFn2 \extra input -> do
        encodeTuple (runFn2 encodeB extra) (runFn2 encodeA extra) $ swap input
  }

-- wrap the newtypes that indicate which local override to use when decoding that type
-- Note: for `coerce` to compile without errors,
-- the constructors of `K0` and `K2` and the `Newtype` class must be imported!
fromExampleRecordType
  :: { stringNormal :: String
     , stringOverride :: String
     , tupleNormal :: Tuple String String
     , tupleSwap :: Tuple String String
     , tupleMultiOverride :: Tuple String String
     }
  -> { stringNormal :: String
     , stringOverride :: K0 "wrapInMathExpr" String
     , tupleNormal :: Tuple String String
     , tupleSwap :: K2 "swapDecoders" (Tuple String String)
     , tupleMultiOverride :: K2 "swapDecoders" (Tuple String (K0 "wrapInMathExpr" String))
     }
fromExampleRecordType = coerce
