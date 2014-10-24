{ cabal, autonixDeps }:

cabal.mkDerivation (self: {
  pname = "autonix-deps-kf5";
  version = "0.1.0.0";
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ autonixDeps ];
  meta = {
    description = "Generate dependencies for KDE 5 Nix expressions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
