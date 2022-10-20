module Json.Primitive.Encode where

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Foreign.Object (Object)

encodeNull :: Json
encodeNull = jsonNull

encodeBoolean :: Boolean -> Json
encodeBoolean = fromBoolean

encodeNumber :: Number -> Json
encodeNumber = fromNumber

encodeString :: String -> Json
encodeString = fromString

encodeArrayPrim :: Array Json -> Json
encodeArrayPrim = fromArray

encodeObjectPrim :: Object Json -> Json
encodeObjectPrim = fromObject
