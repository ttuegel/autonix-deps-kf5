{ mkDerivation, autonix-deps, base, bytestring, conduit, containers
, filepath, lens, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "autonix-deps-kf5";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    autonix-deps base bytestring conduit containers filepath lens mtl
    transformers
  ];
  description = "Generate dependencies for KDE 5 Nix expressions";
  license = stdenv.lib.licenses.bsd3;
}
