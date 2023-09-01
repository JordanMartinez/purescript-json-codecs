let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230826/packages.dhall
        sha256:9ea8909f5f1219bd716b15f04b4d360cacfb32da9d5ae37a550a01c343b9eb10

in  upstream
  with benchotron =
    { dependencies =
      [ "ansi"
      , "arrays"
      , "datetime"
      , "effect"
      , "exceptions"
      , "exists"
      , "foldable-traversable"
      , "formatters"
      , "identity"
      , "integers"
      , "js-date"
      , "lcg"
      , "lists"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "node-process"
      , "node-readline"
      , "node-streams"
      , "now"
      , "ordered-collections"
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
    , version = "0c5342db5caf4608e4c0eb199ec2de3cb95b7d4e"
    }
