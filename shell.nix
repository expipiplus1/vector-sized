{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc882"
, hoogle ? true
}:

(import ./default.nix { inherit pkgs compiler hoogle; }).env
