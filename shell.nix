with import <nixpkgs> {}; 
let sourceFilesOnly = path: type:
    (lib.hasPrefix "var" (toString path)) ||
    (lib.hasPrefix "target" (toString path)) ;
in stdenv.mkDerivation {
    name = "abscissa";
    src = builtins.filterSource sourceFilesOnly ./.;
    buildInputs = [ ghc
      ];
    shellHook = ''
      test -f nix-shell-hook.sh && source nix-shell-hook.sh
    '';
}

