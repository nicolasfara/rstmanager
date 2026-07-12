# flake.nix
{
  description = "Dev shell for rstmanager";

  inputs = {
    # 24.11 provides nodejs_22 (Vite 8 requires Node >= 20.19); 23.05 only had an older nodejs_20.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
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
              nodejs_22
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
