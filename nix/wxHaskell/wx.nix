{ mkDerivation, base, lib, stm, time, wxcore }:
mkDerivation {
  pname = "wx";
  version = "0.93.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base stm time wxcore ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell";
  license = "unknown";
}
