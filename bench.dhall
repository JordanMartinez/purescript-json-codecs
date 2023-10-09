{ name = "json-codecs-benchmarks"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "benchotron"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-readwrite"
  , "gen"
  , "integers"
  , "json"
  , "json-codecs"
  , "lists"
  , "maybe"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall with json-codecs = ./spago.dhall as Location
, sources = [ "bench/**/*.purs" ]
}
