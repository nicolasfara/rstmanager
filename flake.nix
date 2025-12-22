{
  description = "Nix flake for rstmanager development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells."${system}".default = pkgs.mkShell {
          name = "rstmanager-dev";

          # Development tools used by this project
          buildInputs = with pkgs; [
            openjdk21
            sbt
            nodejs-18_x
            yarn
            git
          ];

          # Helpful messages when entering the shell
          shellHook = ''
            echo "Entered rstmanager dev shell (flake devShell)"
            echo "Run: sbt (Scala build), npm run dev (vite), npm install (js deps)"
            echo "Allow direnv in this repo with: direnv allow"
          '';
        };
      });
}
