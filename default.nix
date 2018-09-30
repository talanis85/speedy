{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, directory, disguise, filepath, gitrev, ListZipper, mtl
, optparse-applicative, shakespeare, stdenv, text, time, X11
}:
mkDerivation {
  pname = "speedy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers directory disguise
    filepath gitrev ListZipper mtl optparse-applicative shakespeare
    text time X11
  ];
  description = "Speed Run Timer";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
