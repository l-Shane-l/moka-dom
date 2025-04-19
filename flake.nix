# flake.nix (modified example)
{
  description = "Moka-DOM Development Environment (GHC JS Backend)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # Use a specific commit for real reproducibility
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ ]; config = { }; };

        # The compiler configured using the workaround
        ghcCompilerJs = pkgs.haskell.compiler.ghc9101.override {
          stdenv = pkgs.stdenv.override { targetPlatform = pkgs.pkgsCross.ghcjs.stdenv.targetPlatform; };
        };

        # Create a Haskell package set using this compiler
        jsHaskellPackages = pkgs.haskell.packages.ghc9101.override {
          ghc = ghcCompilerJs;
          # Add other necessary overrides for patched base libs if needed?
        };

        # Define your moka-dom package using this set
        moka-dom-pkg = jsHaskellPackages.callCabal2nix "moka-dom" self { };

      in
      {
        devShells.default = pkgs.mkShell {
          name = "moka-dom-js-dev";
          # Include tools AND dependencies built with the right compiler
          packages = [
            ghcCompilerJs # The compiler itself
            jsHaskellPackages.cabal-install
            pkgs.haskell-language-server # HLS should pick up the right GHC
            # Inherit runtime deps from your package
          ] ++ pkgs.lib.attrValues (import "${moka-dom-pkg}/share/dev-dependencies.nix" { inherit jsHaskellPackages; });


        };

        packages.default = moka-dom-pkg; # Allow building the package too
      }
    );
}
