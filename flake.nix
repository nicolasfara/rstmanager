# flake.nix
{
  description = "Dev shell for rstmanager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells = {
        "${system}" = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              openjdk17
              sbt
              nodejs-18_x
              yarn
              git
            ];

            shellHook = ''
              export JAVA_HOME=${pkgs.openjdk17}
              export PATH=${pkgs.sbt}/bin:$PATH
            '';
          };
        };
      };
    };
}
