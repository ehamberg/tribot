{
  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (nixpkgs.lib) optional;
        pkgs = import nixpkgs { inherit system; };

        ghcWithPkgs = (pkgs.haskellPackages.ghcWithPackages (p: [
            p.HDBC
            p.HDBC-sqlite3
            p.asn1-encoding
            p.asn1-parse
            p.asn1-types
            p.async
            p.base64
            p.basement
            p.bytestring
            p.cereal
            p.connection
            p.cryptonite
            p.hashable
            p.hourglass
            p.hsc2hs
            p.memory
            p.mtl
            p.network
            p.pem
            p.random
            p.socks
            p.stringsearch
            p.text-short
            p.time
            p.tls
            p.utf8-string
            p.x509
            p.x509-store
            p.x509-system
            p.x509-validation
        ]));
        cabal-install = pkgs.cabal-install;
      in
        {
          devShell = pkgs.mkShell {
            buildInputs = [
              ghcWithPkgs
              cabal-install
            ];
          };
        });
}
