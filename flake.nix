{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-for-ghc.url = "github:NixOS/nixpkgs/ebe4301cbd8f81c4f8d3244b3632338bbeb6d49c";
    # 9189ac18287c599860e878e905da550aa6dec1cd
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, nixpkgs-for-ghc, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghc-pkgs = import nixpkgs-for-ghc { inherit system; };
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
              ghc-pkgs.haskell.compiler.ghc984
              (ghc-pkgs.haskell-language-server.override { supportedGhcVersions = [ "984" ]; })
              ghc-pkgs.haskell.packages.ghc984.cabal-fmt
              ghc-pkgs.haskell.packages.ghc984.doctest
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
