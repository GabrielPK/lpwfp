{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    haskell.packages.ghc96.ghc
    haskell.packages.ghc96.stack
    haskellPackages.gnuplot
  ];

  # shellHook = ''
  #   eval $(stack --bash-completion-script stack)
  # '';
}
