{
  description = "prop-solveur, a simple logic solver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        hPkgs = pkgs.haskellPackages;
      in
      {
        formatter = pkgs.nixfmt-rfc-style;

        packages.default =
          let
            rawPackage = pkgs.haskellPackages.callCabal2nix "prop-solveur" ./. { };
          in
          pkgs.haskell.lib.justStaticExecutables (
            rawPackage.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.installShellFiles ];
              postInstall =
                (old.postInstall or "")
                + ''
                  installShellCompletion --cmd prop-solveur \
                      --bash <("$out/bin/prop-solveur" --bash-completion-script "$out/bin/prop-solveur") \
                      --fish <("$out/bin/prop-solveur" --fish-completion-script "$out/bin/prop-solveur") \
                      --zsh  <("$out/bin/prop-solveur" --zsh-completion-script  "$out/bin/prop-solveur")
                '';
            })
          );

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            hPkgs.ghc
            hPkgs.haskell-language-server
            hPkgs.stylish-haskell
          ];
        };
      }
    );
}
