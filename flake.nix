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
          # Using your original GHC version
          ghc = haskell.compiler.ghc967;
          ghc-js = haskell.compiler.ghc967.override
            {
              stdenv = stdenv.override { targetPlatform = pkgsCross.ghcjs.stdenv.targetPlatform; };
            };
          # Using haskell-language-server that matches your GHC version
          hls = haskell-language-server.override {
            supportedGhcVersions = [ "967" ];
          };
        in
        mkShell
          {
            packages = [
              ghc
              ghc-js
              cabal-install
              ghcid
              hello
              hls
              haskellPackages.hoogle
              haskellPackages.hlint
              haskellPackages.implicit-hie # Tool to generate hie.yaml files
            ];
            shellHook = ''
              # Previous stuff
  
              # Set up Emscripten cache directory
              mkdir -p $HOME/.emscripten_cache
              export EM_CACHE=$HOME/.emscripten_cache
  
              # Disable WebGL in Emscripten
              export EM_NO_WEBGL=1
            '';
          };
    };
}
