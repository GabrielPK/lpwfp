{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    haskell.packages.ghc96.stack
    gnuplot
  ];

  # shellHook = ''
  #   eval $(stack --bash-completion-script stack)
  # '';
}
