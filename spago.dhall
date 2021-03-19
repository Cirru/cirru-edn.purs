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
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/Cirru/cirru-edn.purs"
}
