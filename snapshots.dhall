{ name = "json-codecs-snapshots"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "json-codecs"
  , "maybe"
  , "newtype"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall with json-codecs = ./spago.dhall as Location
, sources = [ "snapshots/**/*.purs" ]
}
