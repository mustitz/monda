{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = with pkgs; [
      lean4
      leanPackages.mathlib
      neovim
    ];

  shellHook = ''
    export NVIM_APPNAME=lean4nvim
    alias vim=nvim
  '';
}
