{
  description = "prop-solveur, a simple logic solver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      imports = [ ./nix/overlays.nix ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, self', ... }:
        {
          formatter = pkgs.nixfmt-rfc-style;

          packages.default = (inputs.self.overlays.default pkgs pkgs).prop-solveur;

          devShells.default = pkgs.mkShell {
            packages =
              let
                hPkgs = pkgs.haskellPackages;
              in
              [
                hPkgs.ghc
                pkgs.cabal-install
                hPkgs.cabal-fmt
                hPkgs.haskell-language-server
                hPkgs.fourmolu
              ];
          };

        };

    };

}
