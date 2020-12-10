{ name = "n"
, dependencies =
  [ "bigints"
  , "console"
  , "debug"
  , "effect"
  , "memoize"
  , "node-fs"
  , "parsing"
  , "psci-support"
  , "stringutils"
  ]
, packages = ../../packages.dhall
, sources = [ "Main.purs" ]
}
