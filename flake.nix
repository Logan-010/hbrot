{
  description = "hbrot";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          #Nix has mostly old ass hackage packages so many need to be updated
          #hs = pkgs.haskellPackages.override {
          #  overrides = hself: hsuper: {
          #    pkgname = hself.callHackageDirect {
          #      pkg = "pkgname";
          #      ver = "pkgversion";
          #      sha256 = pkgs.lib.fakeHash;
          #    } { };
          #    pkgname2 = hself.callHackage "pkgname2" "pkgversion2" { };
          #  };
          #};
        in
        {
          # default = hs.callCabal2nix "project" ./. { };
          default = pkgs.haskellPackages.callCabal2nix "project" ./. { };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              # Compiler and build tools
              haskellPackages.ghc
              cabal-install

              # Language server
              haskell-language-server

              # Formatting
              ormolu
              haskellPackages.cabal-fmt

              # Linting
              hlint

              # Utilities
              haskellPackages.ghcid
              git
              pkg-config
            ];
          };
        }
      );
    };
}

