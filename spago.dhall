{ name = "my-project"
, dependencies = [ "console", "effect", "ordered-collections", "psci-support", "cirru-parser" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
