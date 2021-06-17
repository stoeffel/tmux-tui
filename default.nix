let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

in pkgs.haskellPackages.callCabal2nix "tmux-tui" ./. { }
