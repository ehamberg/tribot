{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          tribot = pkgsNew.haskell.lib.justStaticExecutables
            pkgsNew.haskellPackages.tribot;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = let
              oldOverrides = old.overrides or (_: _: { });

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                simpleirc = let
                  src = builtins.fetchGit {
                    url = "https://github.com/dom96/SimpleIrc";
                    ref = "master";
                    rev = "8d156a89801be2c9b6923d85e6b199c8173e445a";
                  };
                in pkgs.haskell.lib.dontCheck
                (haskellPackagesOld.callCabal2nix "simpleirc" src { });
              };

              sourceOverrides =
                pkgsNew.haskell.lib.packageSourceOverrides { tribot = ./.; };

            in pkgsNew.lib.fold pkgsNew.lib.composeExtensions oldOverrides ([
              sourceOverrides
              manualOverrides
            ]);
          });
        };

        pkgs = import nixpkgs {
          inherit config system;
          overlays = [ overlay ];
        };

      in rec {
        packages.default = pkgs.haskellPackages.tribot;

        apps.default = {
          type = "app";
          program = "${pkgs.tribot}/bin/tribot";
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskellPackages.haskell-language-server
              haskellPackages.hlint
              haskellPackages.cabal-fmt
              haskellPackages.ormolu
              cabal-install
              zlib
              sqlite
            ];
          };
        };
      });
}
