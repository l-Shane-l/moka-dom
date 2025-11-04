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
          # We still define ghc 9.10.1 to base the GHCJS compiler on it
          ghcBase = haskell.compiler.ghc9101;
          # Define the GHCJS cross-compiler based on ghcBase
          ghc-js = ghcBase.override
            {
              stdenv = stdenv.override { targetPlatform = pkgsCross.ghcjs.stdenv.targetPlatform; };
            };
        in
        mkShell
          {
            # REMOVE ghc from this list! Only keep ghc-js
            packages = [
              # ghc  # <--- REMOVE THIS LINE
              ghc-js
              cabal-install
              ghcid
              emscripten
              pkgs.haskell.packages.ghc9101.haskell-language-server
              pkgs.haskell.packages.ghc9101.implicit-hie
              pkgs.nodejs # Often needed by GHCJS builds/runtime
            ];
            shellHook = ''
              alias ghcjs=javascript-unknown-ghcjs-ghc

              # Keep the emscripten cache setup
              if [ ! -d "$(pwd)/.emscripten_cache" ]; then
                # Use -L to copy symlinks as symlinks if needed, R for recursive
                cp -LR "${emscripten}/share/emscripten/cache/" "$(pwd)/.emscripten_cache"
                chmod -R u+rwX "$(pwd)/.emscripten_cache"
              fi
              # Ensure EM_CACHE is always set when the shell starts
              export EM_CACHE="$(pwd)/.emscripten_cache"

              # Optional: Add GHCJS bin to PATH explicitly, though Nix usually handles it
              # export PATH="${ghc-js}/bin:$PATH"
              # echo "GHCJS compiler: $(which javascript-unknown-ghcjs-ghc)"
            '';
          };
    };
}
