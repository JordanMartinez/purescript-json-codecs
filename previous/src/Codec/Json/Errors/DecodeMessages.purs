module Codec.Json.Errors.DecodeMessages where

import Prelude

numToIntConversionFailure :: Number -> String
numToIntConversionFailure = append "Could not convert Number to Int: " <<< show

stringToCharConversionFailure :: String -> String
stringToCharConversionFailure = append "Could not get char at index 0 in String: "

stringNotEmptyFailure :: String
stringNotEmptyFailure = "Received empty string"

arrayNotEmptyFailure :: String
arrayNotEmptyFailure = "Received empty array"
