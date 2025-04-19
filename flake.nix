{
  description = "A pristine single page web app example written in Haskell.";

  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs";
    };

  outputs = { self, nixpkgs }:
    {
      devShells.x86_64-linux.default = with import nixpkgs { system = "x86_64-linux"; }; with pkgs;
        let
          ghc = haskell.compiler.ghc9101;
          ghc-js = haskell.compiler.ghc9101.override
            {
              stdenv = stdenv.override { targetPlatform = pkgsCross.ghcjs.stdenv.targetPlatform; };
            };
        in
        mkShell
          {
            packages = [ ghc ghc-js cabal-install ghcid hello ];
            shellHook = "alias ghcjs=javascript-unknown-ghcjs-ghc";
          };
    };
}
