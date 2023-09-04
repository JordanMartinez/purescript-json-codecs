{ name = "json-codecs-benchmarks"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "effect"
  , "foldable-traversable"
  , "json-codecs"
  , "maybe"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall with json-codecs = ./spago.dhall as Location
, sources = [ "snapshots/**/*.purs" ]
}
