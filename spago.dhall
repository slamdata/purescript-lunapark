{ name = "lunapark"
, dependencies =
  [ "argonaut-core"
  , "argonaut-codecs"
  , "affjax"
  , "console"
  , "css"
  , "effect"
  , "node-fs-aff"
  , "psci-support"
  , "run"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
