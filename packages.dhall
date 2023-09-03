let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230903/packages.dhall
        sha256:92eae152a01cd49a0d1ec82dd487700870f50f6f5676ea59694ff9decc54648d

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
