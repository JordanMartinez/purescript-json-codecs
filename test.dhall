let soureConfig = ./spago.dhall
in 
{ name = "json-codecs-test"
, dependencies = soureConfig.dependencies #
  [ "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "dodo-printer"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "strings"
  , "these"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "test/**/*.purs" ] # soureConfig.sources
}
