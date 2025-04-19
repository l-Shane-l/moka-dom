# flake.nix (Minimal GHC JS Backend Setup Attempt)
{
  description = "Minimal GHC JS Backend Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # Pin this later!
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ ]; config = { }; };

        # Select GHC version (try 9.8 or 9.6 if 9.10 causes issues)
        ghcVersion = "ghc98"; # Let's try 9.8 first, maybe more stable

        # Attempt to configure GHC for JS backend - METHOD 1 (Using ghc.override)
        # This is speculative, the exact attribute might differ or not exist
        ghcCompilerJs = pkgs.haskell.compiler.${ghcVersion}.override {
          target = "javascript-unknown-ghcjs"; # The direct way, if supported
        };

        # If Method 1 fails, try Method 2 (Cross compilation setup)
        # This assumes nixpkgs has specific setup for this target
        # ghcCompilerJs = pkgs.pkgsCross.ghcjs.haskell.compiler.${ghcVersion};

      in
      {
        devShells.default = pkgs.mkShell {
          name = "ghc-js-test-shell";
          packages = [
            ghcCompilerJs # Just the compiler for now
            pkgs.cabal-install # A standard cabal-install
            pkgs.git
          ];
        };
      }
    );
}
