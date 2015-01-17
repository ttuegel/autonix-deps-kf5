with (import <nixpkgs> {});
with pkgs;
(haskellngPackages.callPackage ./. {
  autonix-deps = haskellngPackages.callPackage ./autonix-deps {
    libarchive-conduit =
      haskellngPackages.callPackage
        ./libarchive-conduit
        { archive = pkgs.libarchive; };
  };
}).env
