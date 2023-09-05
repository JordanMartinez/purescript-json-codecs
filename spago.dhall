{ name = "json-codecs"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "bifunctors"
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
, sources = [ "src/**/*.purs" ]
}
