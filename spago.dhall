{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-erl-cowboy"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-ranch"
  , "erl-tuples"
  , "foreign"
  , "functions"
  , "maybe"
  , "prelude"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
