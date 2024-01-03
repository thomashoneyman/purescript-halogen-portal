{ name = "halogen-portal"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "halogen-store"
  , "maybe"
  , "prelude"
  , "transformers"
  , "typelevel-prelude"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
