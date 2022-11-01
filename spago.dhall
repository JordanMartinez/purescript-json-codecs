{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
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
  , "prelude"
  , "profunctor"
  , "record"
  , "safe-coerce"
  , "strings"
  , "these"
  , "tuples"
  , "unsafe-coerce"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
