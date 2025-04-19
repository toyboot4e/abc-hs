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
            rev = "f385e71ba270716f5a94e3ed9bd23a24f78799d0";
            sha256 = "sha256-7pbCTgWt+khKVyMV03HanvuOX2uAC0PL9OLmqly7IWE=";
          };
          npmDepsHash = "sha256-ufG7Fq5D2SOzUp8KYRYUB5tYJYoADuhK+2zDfG0a3ks=";
          npmPackFlags = [ "--ignore-scripts" ];
          NODE_OPTIONS = "--openssl-legacy-provider";
        };
        oj-verify =
          with pkgs.python3Packages;
          pkgs.python3Packages.buildPythonApplication {
            name = "verification-helper";
            version = "5.6.0";
            pyproject = true;
            src = pkgs.fetchFromGitHub {
              owner = "online-judge-tools";
              repo = "verification-helper";
              rev = "adbff121b1f96de5f34e9f1483eb47d661c54075";
              fetchSubmodules = false;
              sha256 = "sha256-f7Ge8kLRQv9uxdNGtgNsypGVY0XAnKPCg8HYQ5nT6mI=";
            };
            build-system = [ setuptools ];
            dependencies = [
              colorlog
              importlab
              online-judge-tools
              pyyaml
              setuptools
              toml
            ];
            propagatedBuildInputs = [ setuptools ];
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
              oj-verify
              just

              # TODO: needed?
              python312Packages.selenium
              python312Packages.pyaml
              python312Packages.importlab
              # python312Packagesz.sxsdiff # for oj side-by-side diff
              # nodejs

              hlint
              haskell.compiler.ghc947
              (haskell-language-server.override { supportedGhcVersions = [ "947" ]; })
              haskell.packages.ghc947.cabal-fmt
              haskell.packages.ghc947.doctest
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
