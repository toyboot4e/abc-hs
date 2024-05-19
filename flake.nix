{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = with pkgs; mkShell {
          nativeBuildInputs = [
            pkg-config
            stack
            cabal-install
            # glibc
            llvmPackages.bintools
            # openssl
          ];
        
          packages = [
            # atcoder-cli is from npm
        
            online-judge-tools
            python311Packages.selenium
            python311Packages.yaml
            nodejs
        
            hlint
            haskell.compiler.ghc945
            (haskell-language-server.override { supportedGhcVersions = [ "945" ]; })
            haskellPackages.hoogle
        
            # stable.haskell.compiler.ghc943
            # (stable.haskell-language-server.override { supportedGhcVersions = [ "943" ]; })
        
            haskellPackages.ghcid
            haskellPackages.ghcide
          ];
        
	  # shell.nix:
          # buildInputs = [ ];
          # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      });
}
