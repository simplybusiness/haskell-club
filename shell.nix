with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "haskell-club";
  version = "0.0.1";
  buildInputs = [ ghc ];
}
