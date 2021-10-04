{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-erl-cowboy"
, dependencies =
  [ "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-ranch"
  , "erl-ssl"
  , "erl-tuples"
  , "foreign"
  , "functions"
  , "maybe"
  , "prelude"
  , "record"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
