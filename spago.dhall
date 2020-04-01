{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "control"
    , "datetime"
    , "effect"
    , "foldable-traversable"
    , "free"
    , "freet"
    , "js-date"
    , "lists"
    , "maybe"
    , "psci-support"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
