{ name = "json-codecs"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "bifunctors"
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
, sources = [ "src/**/*.purs" ]
}
