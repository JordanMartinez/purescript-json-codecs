let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230820/packages.dhall
        sha256:f002e9182da135bb48f3f1937830c57a66a422e21fc03fa0922755e8b444de66

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
  with dodo-printer =
    { repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v2.2.1"
    , dependencies =
      [ "aff"
      , "ansi"
      , "arrays"
      , "avar"
      , "console"
      , "control"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "integers"
      , "lists"
      , "maybe"
      , "minibench"
      , "newtype"
      , "node-buffer"
      , "node-child-process"
      , "node-fs-aff"
      , "node-path"
      , "node-process"
      , "node-streams"
      , "parallel"
      , "partial"
      , "posix-types"
      , "prelude"
      , "safe-coerce"
      , "strings"
      ]
    }
  with json =
    { dependencies =
    [ "prelude"
    , "functions"
    , "integers"
    , "maybe"
    , "either"
    , "tuples"
    , "foldable-traversable"
    , "gen"
    , "strings"
    , "unfoldable"
    ]
    , repo = "https://github.com/purescript/purescript-json.git"
    , version = "b06c9d5bddaf4dc11b09b94bc31c379722396648"
    }
