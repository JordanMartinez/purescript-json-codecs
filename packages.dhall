let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

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
