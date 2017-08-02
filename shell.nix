with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "haskell-club";
  version = "0.0.1";
  buildInputs = [ ghc cabal-install zlib pkgconfig ];
  shellHook = ''
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${zlib}/lib
  '';
}
