module Test.Codec.Json.Unidirectional.Typeclass.LocalOverrides.Decoding where

import Prelude

import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Either (either)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), swap)
import Effect (Effect)
import Effect.Class.Console (log)
import Codec.Json.Errors.AnsiDodoError (printAnsiDodoError, runJsonDecoderADE')
import Codec.Json.Newtypes (K0(..), K2(..))
import Codec.Json.Unidirectional.Decode.Class (ExistentialDecoder0, ExistentialDecoder2, decodeJson, mkExistentialDecoder0, mkExistentialDecoder2)
import Codec.Json.Unidirectional.Decode.Value (decodeString, decodeTuple)
import Codec.Json.Unidirectional.Encode.Class (encodeJson)
import Safe.Coerce (coerce)

runOutput :: Effect Unit
runOutput = do
  log "\n### Typeclass - Local Overrides - Decoding Output:"
  log "Starting record: "
  log $ prettyPrintRecord recordValue
  let jsonValue = encodeJson recordValue
  log "Encoded Json: "
  log $ stringifyWithIndent 2 jsonValue
  log "Normal decoded value: "
  log
    $ either printAnsiDodoError prettyPrintRecord
    $ runJsonDecoderADE' unit jsonValue decodeJson
  log "Locally-overridden decoded value: "
  log
    $ either printAnsiDodoError (prettyPrintRecord <<< toExampleRecordType)
    $ runJsonDecoderADE' localDecodeOverrides jsonValue decodeJson
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

newtype LocalDecodeOverrides = LocalDecodeOverrides
  { wrapInMathExpr :: ExistentialDecoder0 String
  , swapDecoders :: ExistentialDecoder2 Tuple
  }

derive instance Newtype LocalDecodeOverrides _

localDecodeOverrides :: LocalDecodeOverrides
localDecodeOverrides = LocalDecodeOverrides
  { wrapInMathExpr: mkExistentialDecoder0 $ map (\plus -> "1 " <> plus <> " 1 == 2") decodeString
  , swapDecoders: mkExistentialDecoder2 \decodeA decodeB -> swap <$> decodeTuple decodeA decodeB
  }

-- unwrap the newtypes that indicate which local override to use when decoding that type
-- Note: for `coerce` to compile without errors,
-- the constructors of `K0` and `K2` and the `Newtype` class must be imported!
toExampleRecordType
  :: { stringNormal :: String
     , stringOverride :: K0 "wrapInMathExpr" String
     , tupleNormal :: Tuple String String
     , tupleSwap :: K2 "swapDecoders" (Tuple String String)
     , tupleMultiOverride :: K2 "swapDecoders" (Tuple String (K0 "wrapInMathExpr" String))
     }
  -> { stringNormal :: String
     , stringOverride :: String
     , tupleNormal :: Tuple String String
     , tupleSwap :: Tuple String String
     , tupleMultiOverride :: Tuple String String
     }
toExampleRecordType = coerce
