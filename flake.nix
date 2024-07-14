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
            python311Packages.pyaml
            # python311Packagesz.sxsdiff # for oj side-by-side diff
            python311Packages.importlab
            nodejs

            hlint
            # haskell.compiler.ghc946
            # (haskell-language-server.override { supportedGhcVersions = [ "946" ]; })
            haskell.compiler.ghc963
            (haskell-language-server.override { supportedGhcVersions = [ "963" ]; })
            haskellPackages.hoogle

            haskellPackages.ghcid
            haskellPackages.ghcide

            haskellPackages.ghci-dap
            haskellPackages.haskell-dap
            haskellPackages.haskell-debug-adapter
          ];

	  # shell.nix:
          # buildInputs = [ ];
          # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      });
}
