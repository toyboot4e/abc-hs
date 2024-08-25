{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-oj.url = "github:NixOS/nixpkgs/3f316d2a50699a78afe5e77ca486ad553169061e";
  };

  outputs = { nixpkgs, flake-utils, nixpkgs-oj, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs-oj = import nixpkgs-oj {
          inherit system;
        };
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

            # online-judge-tools
            pkgs-oj.online-judge-tools
            # python311Packages.selenium
            # python311Packages.pyaml
            # python311Packages.importlab
            # python311Packagesz.sxsdiff # for oj side-by-side diff
            nodejs

            hlint

            # lts-21.6
            haskell.compiler.ghc946
            (haskell-language-server.override { supportedGhcVersions = [ "946" ]; })

            # lts-21.15
            # haskell.compiler.ghc947
            # (haskell-language-server.override { supportedGhcVersions = [ "947" ]; })

            # lts-22.0
            # haskell.compiler.ghc963
            # (haskell-language-server.override { supportedGhcVersions = [ "963" ]; })
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
