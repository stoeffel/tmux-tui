let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

in pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.entr
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.haskellPackages.ormolu
  ];
}
