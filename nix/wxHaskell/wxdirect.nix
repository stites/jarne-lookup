{ mkDerivation, base, containers, directory, filepath, lib, parsec
, process, strict, time
}:
mkDerivation {
  pname = "wxdirect";
  version = "0.93.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base containers directory filepath parsec process strict time
  ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "helper tool for building wxHaskell";
  license = lib.licenses.bsd3;
  mainProgram = "wxdirect";
}
