{ nixpkgs ? import ../../../../.. {}
, haskellPackages ? nixpkgs.haskellPackages
}:

haskellPackages.callPackage ./default.nix {
  inherit (nixpkgs.autonix) deps;
}
