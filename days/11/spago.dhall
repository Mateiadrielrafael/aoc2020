{ name = "n"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "parsing"
  , "psci-support"
  , "sized-vectors"
  , "stringutils"
  ]
, packages = ../../packages.dhall
, sources = [ "Main.purs" ]
}
