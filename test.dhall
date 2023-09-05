let soureConfig = ./spago.dhall
in 
{ name = "json-codecs-test"
, dependencies = soureConfig.dependencies #
  [ "argonaut-core"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
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
