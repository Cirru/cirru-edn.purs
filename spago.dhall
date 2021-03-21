{ name = "cirru-edn"
, dependencies =
  [ "arrays"
  , "cirru-parser"
  , "console"
  , "effect"
  , "either"
  , "ordered-collections"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/Cirru/cirru-edn.purs"
}
