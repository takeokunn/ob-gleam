{
  description = "ob-gleam - Org Babel support for Gleam";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.emacs
            pkgs.gleam
            pkgs.erlang
            pkgs.rebar3
            pkgs.nodejs
          ];

          shellHook = ''
            echo "ob-gleam development shell"
            echo "  emacs: $(emacs --version | head -1)"
            echo "  gleam: $(gleam --version)"
            echo ""
            echo "Commands:"
            echo "  make compile       Byte-compile ob-gleam.el"
            echo "  make test          Run ERT tests"
            echo "  make lint          Run checkdoc"
            echo "  make package-lint  Run package-lint"
            echo "  nix flake check    Run all checks"
            echo ""
            echo "Examples (C-c C-c the Setup block first, then C-c C-c on Gleam blocks):"
            echo "  emacs -Q -L . example/basic.org        Basic hello world"
            echo "  emacs -Q -L . example/full-module.org   Full module with pub fn main()"
            echo "  emacs -Q -L . example/imports.org       Using :imports header"
            echo "  emacs -Q -L . example/deps.org          Using :deps header (needs network)"
            echo "  emacs -Q -L . example/javascript-target.org  Using :target javascript (needs Node.js)"
          '';
        };
      });

      checks = eachSystem (pkgs:
        let
          emacs = pkgs.emacs;
          emacsWithOrg = (pkgs.emacsPackagesFor emacs).emacsWithPackages (p: [ p.org ]);
          src = pkgs.lib.cleanSource ./.;
        in
        {
          compile = pkgs.stdenvNoCC.mkDerivation {
            name = "ob-gleam-compile";
            inherit src;
            nativeBuildInputs = [ emacsWithOrg ];
            env.EMACS = pkgs.lib.getExe emacsWithOrg;
            buildPhase = ''
              make compile
            '';
            installPhase = ''
              touch $out
            '';
          };

          test = pkgs.stdenvNoCC.mkDerivation {
            name = "ob-gleam-test";
            inherit src;
            nativeBuildInputs = [ emacsWithOrg ];
            env.EMACS = pkgs.lib.getExe emacsWithOrg;
            buildPhase = ''
              make test
            '';
            installPhase = ''
              touch $out
            '';
          };
        });
    };
}
