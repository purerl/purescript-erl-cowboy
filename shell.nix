let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "c00959877fb06b09468562518b408acda886c79e";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "b3f10cd33107f220e4328f0222d3d026bf4f5f99";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "https://github.com/id3as/nixpkgs-purerl-support.git";
      rev = "47a8bd6ff017dad2208f10dddf91f6f3258a09be";
    };

  erlangReleases = builtins.fetchGit {
    name = "nixpkgs-nixerl";
    url = "https://github.com/nixerl/nixpkgs-nixerl";
    rev = "6321e5b8b6cfe4c13307a2d2093c4b6243e6ad53";
  };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import purerlSupport)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    # Purescript - we use a specific version rather than
    # whatever the latest is exposed via nixpkgs
    purerl-support.purescript-0-13-8
    purerl-support.spago-0-16-0
    purerl-support.dhall-json-1-5-0

    # Purerl backend for purescript
    purerl.purerl-0-0-7

    (nixerl.erlang-23-0-4.erlang.override { wxSupport = false; })
  ];
}
