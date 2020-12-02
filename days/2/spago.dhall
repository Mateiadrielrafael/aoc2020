{ name = "2"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "parsing"
  , "psci-support"
  , "stringutils"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
