{ name = "my-project"
, dependencies =
  [ "ansi"
  , "argonaut-core"
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
  , "invariant"
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
  , "st"
  , "strings"
  , "these"
  , "tuples"
  , "uncurried-transformers"
  , "unsafe-coerce"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
