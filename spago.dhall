{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "clio"
, dependencies = [ "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
}
