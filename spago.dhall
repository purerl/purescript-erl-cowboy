{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-erl-cowboy"
, dependencies = [
  "console",
  "effect",
  "erl-atom",
  "erl-binary",
  "erl-lists",
  "erl-maps",
  "erl-tuples",
  "erl-modules",
  "erl-kernel",
  "foreign"
 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"  ]
, backend = "purerl"
}
