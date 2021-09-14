{ name = "logic"
, dependencies =
  [ "arrays"
  , "catenable-lists"
  , "control"
  , "either"
  , "machines"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
