{ nixpkgs ? import <nixpkgs> {}
, haskellPackages ? nixpkgs.haskellPackages
}:

haskellPackages.callPackage ./default.nix {
  autonixDeps = haskellPackages.callPackage ../autonix-deps {};
}
