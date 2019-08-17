{ name =
    "halogen-portal"
, dependencies =
    [ "aff-bus", "effect", "halogen", "psci-support", "record" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
