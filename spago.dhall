{ name = "my-project"
, dependencies =
  [ "arrays"
  , "cirru-parser"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "ordered-collections"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
