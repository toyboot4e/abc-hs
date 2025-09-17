{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        # https://github.com/lmdexpr/contest/blob/d7d7e84034cf1ce6e54a59ffb0435e5edafa873e/flake.nix#L83C1-L95C11
        atcoder-cli = pkgs.buildNpmPackage {
          pname = "atcoder-cli";
          version = "2.2.0";
          src = pkgs.fetchFromGitHub {
            owner = "Tatamo";
            repo = "atcoder-cli";
            tag = "v2.2.0";
            hash = "sha256-7pbCTgWt+khKVyMV03HanvuOX2uAC0PL9OLmqly7IWE=";
          };
          npmDepsHash = "sha256-ufG7Fq5D2SOzUp8KYRYUB5tYJYoADuhK+2zDfG0a3ks";
          npmFlags = [ "--ignore-scripts" ];
          NODE_OPTIONS = "--openssl-legacy-provider";
        };
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              pkg-config
              stack
              cabal-install
              llvmPackages.bintools
            ];

            packages = [
              atcoder-cli
              online-judge-tools
              online-judge-verify-helper
              just

              # TODO: needed?
              python312Packages.selenium
              python312Packages.pyaml
              python312Packages.importlab
              # python312Packagesz.sxsdiff # for oj side-by-side diff
              # nodejs

              hlint
              haskell.compiler.ghc948
              (haskell-language-server.override { supportedGhcVersions = [ "948" ]; })
              haskell.packages.ghc948.cabal-fmt
              haskell.packages.ghc948.doctest
              haskellPackages.hoogle
              haskellPackages.ghcid
              haskellPackages.ghcide
              haskellPackages.ghci-dap
              haskellPackages.haskell-dap
              haskellPackages.haskell-debug-adapter
              haskellPackages.implicit-hie
            ];
          };
        shellHook = ''
          acc config oj-path $(which oj)
        '';
      }
    );
}
