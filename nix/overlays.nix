{

  flake.overlays.default =
    final: _:

    let
      rawPackage = final.haskellPackages.callCabal2nix "prop-solveur" ../. { };

      prop-solveur = final.lib.pipe rawPackage [
        (
          p:
          p.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.installShellFiles ];
            postInstall =
              (old.postInstall or "")
              + ''
                installShellCompletion --cmd prop-solveur \
                    --bash <("$out/bin/prop-solveur" --bash-completion-script "$out/bin/prop-solveur") \
                    --fish <("$out/bin/prop-solveur" --fish-completion-script "$out/bin/prop-solveur") \
                    --zsh  <("$out/bin/prop-solveur" --zsh-completion-script  "$out/bin/prop-solveur")
              '';
          })
        )

        final.haskell.lib.justStaticExecutables
      ];
    in

    {
      inherit prop-solveur;
    };

}
