{ mkDerivation, array, base, bytestring, containers, directory
, filepath, lib, parsec, stm, time, wxc, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxcore";
  version = "0.93.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers directory filepath parsec stm time
  ];
  librarySystemDepends = [ wxc ];
  libraryPkgconfigDepends = [ wxc wxGTK ];
  libraryToolDepends = [ wxdirect ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell core";
  license = "unknown";
}
