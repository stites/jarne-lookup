{ mkDerivation, array, base, Cabal, cabal-macosx, containers
, executable-path, filepath, lib, process, random, reactive-banana
, wx, wxcore
}:
mkDerivation {
  pname = "reactive-banana-wx";
  version = "1.1.1.0";
  src = ./.;
  configureFlags = [ "-fbuildexamples" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal cabal-macosx ];
  libraryHaskellDepends = [ base reactive-banana wx wxcore ];
  executableHaskellDepends = [
    array base containers executable-path filepath process random
    reactive-banana wx wxcore
  ];
  homepage = "http://wiki.haskell.org/Reactive-banana";
  description = "Examples for the reactive-banana library, using wxHaskell";
  license = lib.licenses.bsd3;
}
