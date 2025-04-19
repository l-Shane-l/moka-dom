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
          ghc = haskell.compiler.ghc9101;
          ghc-js = haskell.compiler.ghc9101.override
            {
              stdenv = stdenv.override { targetPlatform = pkgsCross.ghcjs.stdenv.targetPlatform; };
            };
          # Using haskell-language-server that matches your GHC version
          hls = haskell-language-server.override {
            supportedGhcVersions = [ "9101" ];
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
                            alias ghcjs=javascript-unknown-ghcjs-ghc
              
                            # Create or update cabal.project to disable problematic flags
                            cat > cabal.project <<EOF
              packages: .

              -- Disable any problematic flags
              package *
                ghc-options: -v2
              EOF

                            echo "To generate hie.yaml, run: gen-hie > hie.yaml"
            '';
          };
    };
}
