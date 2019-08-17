let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190815/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190815/src/packages.dhall sha256:6ca4c07251e87e4e688609af4c9dfbf01e77a225f2b5001ba321cfdc39bf1c0f

let overrides = {=}

let additions = 
      { halogen-storybook =
          { dependencies =
              [ "halogen"
              , "routing"
              , "foreign-object"
              ]
          , repo =
              "https://github.com/rnons/purescript-halogen-storybook"
          , version =
              "master"
          }
      }

in  upstream // overrides // additions
