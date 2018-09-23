{ mkDerivation, aeson, base, bytestring, containers, directory
, disguise, filepath, ListZipper, mtl, stdenv, text, time, X11
}:
mkDerivation {
  pname = "speedy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory disguise filepath
    ListZipper mtl text time X11
  ];
  description = "Speed Run Timer";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
