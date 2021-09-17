let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.6-20200715/src/packages.dhall sha256:98227ee8945a3c7ca4a105c1001dd067128e504bdd45cb6bd9c4509563d83fb9

let overrides = {=}

let additions =
      { erl-kernel =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-process"
          , "erl-tuples"
          , "erl-untagged-union"
          , "foldable-traversable"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-kernel.git"
        , version = "2c1f78a3aa6993e91e342a984c522b87b98bbb2b"
        }
      }

in  upstream // overrides // additions
