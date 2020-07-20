let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.6-20200715/src/packages.dhall sha256:98227ee8945a3c7ca4a105c1001dd067128e504bdd45cb6bd9c4509563d83fb9

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
