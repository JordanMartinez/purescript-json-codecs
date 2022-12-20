{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "arrays"
  , "benchotron"
  , "codec"
  , "codec-argonaut"
  , "effect"
  , "foldable-traversable"
  , "formatters"
  , "functions"
  , "json-codecs"
  , "lists"
  , "node-path"
  , "now"
  , "prelude"
  , "quickcheck"
  ]
, packages =
    ./packages.dhall
  with benchotron =
    { dependencies =
      [ "ansi"
      , "arrays"
      , "codec"
      , "datetime"
      , "effect"
      , "exceptions"
      , "exists"
      , "foldable-traversable"
      , "functions"
      , "identity"
      , "integers"
      , "js-date"
      , "lcg"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "node-path"
      , "node-process"
      , "node-readline"
      , "node-streams"
      , "now"
      , "partial"
      , "prelude"
      , "profunctor"
      , "quickcheck"
      , "strings"
      , "transformers"
      , "tuples"
      , "unfoldable"
      ]
    , repo = "https://github.com/JordanMartinez/purescript-benchotron.git"
    , version = "ad76b5e1282cb9d7aaaca3d146c5014ae962cb6e"
    }
  with json-codecs = ./spago.dhall as Location
, sources = [ "bench/**/*.purs" ]
}
