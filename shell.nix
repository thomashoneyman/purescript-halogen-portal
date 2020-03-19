let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz";
  };

  pkgs = import nixpkgs {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    # 2019-07-06
    rev = "9a8d138663c5d751e3a84f1345166e1f0f760a07";
    sha256 = "1c0mqn4wxh4bmxnf6hgrhk442kl2m9y315wik87wrw2ikb7s1szf";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "halogen-portal";
  buildInputs = [
    easy-ps.purs
    easy-ps.spago
    easy-ps.purty
    pkgs.yarn
    pkgs.nodejs-11_x
  ];
}
