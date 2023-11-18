{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let ghc = haskellPackages.ghcWithPackages (ps: [ ps.fourmolu ps.cabal-fmt ]);
in mkShell {
  buildInputs = [ ghcid ghc cabal-install haskell-language-server ];
  ENV_VAR = "env_var";
}
