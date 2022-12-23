{ name = "json-codecs-benchmarks"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "benchotron"
  , "codec"
  , "codec-argonaut"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "foreign-readwrite"
  , "functions"
  , "json-codecs"
  , "maybe"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "unsafe-coerce"
  ]
, packages =
    ./packages.dhall
  with json-codecs = ./spago.dhall as Location
, sources = [ "bench/**/*.purs" ]
}
