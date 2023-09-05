{ name = "json-codecs-snapshots"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "json-codecs"
  , "lists"
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
