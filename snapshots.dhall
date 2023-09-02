{ name = "json-codecs-benchmarks"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "benchotron"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "json-codecs"
  , "maybe"
  , "newtype"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall with json-codecs = ./spago.dhall as Location
, sources = [ "snapshots/**/*.purs" ]
}
